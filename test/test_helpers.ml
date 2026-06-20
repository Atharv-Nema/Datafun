open Datafun_lib
open Ast
open Type_checker
open Pretty

let show_fin_opt = function
  | None   -> "None"
  | Some t -> "Some(" ^ show_fin t ^ ")"

let show_lat_opt = function
  | None   -> "None"
  | Some l -> "Some(" ^ show_lattice l ^ ")"

let show_ctx ctx =
  "[" ^ String.concat "; "
    (List.map (fun (x, q, _) ->
       x ^ ":" ^ (match q with Disc -> "D" | Ord -> ".")) ctx)
  ^ "]"

(* --- typ_to_fin --- *)

let%expect_test "typ_to_fin: unit" =
  print_string (show_fin_opt (typ_to_fin TUnit));
  [%expect {| Some(unit) |}]

let%expect_test "typ_to_fin: int" =
  print_string (show_fin_opt (typ_to_fin TInt));
  [%expect {| Some(int) |}]

let%expect_test "typ_to_fin: int * unit" =
  print_string (show_fin_opt (typ_to_fin (TProd (TInt, TUnit))));
  [%expect {| Some((int * unit)) |}]

let%expect_test "typ_to_fin: int + unit" =
  print_string (show_fin_opt (typ_to_fin (TSum (TInt, TUnit))));
  [%expect {| Some((int + unit)) |}]

let%expect_test "typ_to_fin: set int" =
  print_string (show_fin_opt (typ_to_fin (TPow FInt)));
  [%expect {| Some(set int) |}]

let%expect_test "typ_to_fin: [int]" =
  print_string (show_fin_opt (typ_to_fin (TBox TInt)));
  [%expect {| Some([int]) |}]

let%expect_test "typ_to_fin: int -> int is not finite" =
  print_string (show_fin_opt (typ_to_fin (TFun (TInt, TInt))));
  [%expect {| None |}]

let%expect_test "typ_to_fin: (int -> int) * int is not finite" =
  print_string (show_fin_opt (typ_to_fin (TProd (TFun (TInt, TInt), TInt))));
  [%expect {| None |}]

(* --- typ_to_lattice --- *)

let%expect_test "typ_to_lattice: unit" =
  print_string (show_lat_opt (typ_to_lattice TUnit));
  [%expect {| Some(unit) |}]

let%expect_test "typ_to_lattice: unit * set int" =
  print_string (show_lat_opt (typ_to_lattice (TProd (TUnit, TPow FInt))));
  [%expect {| Some((unit * set int)) |}]

let%expect_test "typ_to_lattice: set int" =
  print_string (show_lat_opt (typ_to_lattice (TPow FInt)));
  [%expect {| Some(set int) |}]

let%expect_test "typ_to_lattice: int is not a lattice" =
  print_string (show_lat_opt (typ_to_lattice TInt));
  [%expect {| None |}]

let%expect_test "typ_to_lattice: int -> int is not a lattice" =
  print_string (show_lat_opt (typ_to_lattice (TFun (TInt, TInt))));
  [%expect {| None |}]

let%expect_test "typ_to_lattice: int + unit is not a lattice" =
  print_string (show_lat_opt (typ_to_lattice (TSum (TInt, TUnit))));
  [%expect {| None |}]

(* --- restrict --- *)

let%expect_test "restrict: drops Ord bindings" =
  let ctx = [("x", Ord, TInt); ("y", Disc, TUnit)] in
  print_string (show_ctx (restrict ctx));
  [%expect {| [y:D] |}]

let%expect_test "restrict: empty ctx" =
  print_string (show_ctx (restrict []));
  [%expect {| [] |}]

let%expect_test "restrict: all-Disc ctx is unchanged" =
  let ctx = [("x", Disc, TInt); ("y", Disc, TUnit)] in
  print_string (show_ctx (restrict ctx));
  [%expect {| [x:D; y:D] |}]

let%expect_test "restrict: all-Ord ctx becomes empty" =
  let ctx = [("x", Ord, TInt); ("y", Ord, TUnit)] in
  print_string (show_ctx (restrict ctx));
  [%expect {| [] |}]

(* --- fin_to_typ --- *)

let%expect_test "fin_to_typ: unit" =
  print_string (show_typ (fin_to_typ FUnit));
  [%expect {| unit |}]

let%expect_test "fin_to_typ: int" =
  print_string (show_typ (fin_to_typ FInt));
  [%expect {| int |}]

let%expect_test "fin_to_typ: product" =
  print_string (show_typ (fin_to_typ (FProd (FInt, FUnit))));
  [%expect {| (int * unit) |}]

let%expect_test "fin_to_typ: sum" =
  print_string (show_typ (fin_to_typ (FSum (FInt, FUnit))));
  [%expect {| (int + unit) |}]

let%expect_test "fin_to_typ: power set" =
  print_string (show_typ (fin_to_typ (FPow FInt)));
  [%expect {| set int |}]

let%expect_test "fin_to_typ: box" =
  print_string (show_typ (fin_to_typ (FBox FInt)));
  [%expect {| [int] |}]

(* --- lattice_to_typ --- *)

let%expect_test "lattice_to_typ: unit" =
  print_string (show_typ (lattice_to_typ LUnit));
  [%expect {| unit |}]

let%expect_test "lattice_to_typ: product" =
  print_string (show_typ (lattice_to_typ (LProd (LUnit, LPow FInt))));
  [%expect {| (unit * set int) |}]

let%expect_test "lattice_to_typ: power set" =
  print_string (show_typ (lattice_to_typ (LPow FInt)));
  [%expect {| set int |}]

(* --- Alpha.rename ---
   We use [create_counter] to get a counter starting at 0, giving deterministic
   fresh names of the form "base_N". *)

let%expect_test "rename: free variable is not renamed" =
  let c = Alpha.create_counter () in
  print_string (show_expr (Alpha.rename c [] (Var "x")));
  [%expect {| (Var x) |}]

let%expect_test "rename: lambda binder is freshened and body updated" =
  let c = Alpha.create_counter () in
  let e = Alpha.rename c [] (Lam { x = "x"; a = TInt; e = Var "x" }) in
  print_string (show_expr e);
  [%expect {| (Lam x_0 : int . (Var x_0)) |}]

let%expect_test "rename: shadowing gives distinct fresh names" =
  (* fun (x:int) -> fun (x:int) -> x  — outer gets x_0, inner gets x_1 *)
  let c = Alpha.create_counter () in
  let e = Alpha.rename c []
    (Lam { x = "x"; a = TInt;
           e = Lam { x = "x"; a = TInt; e = Var "x" } }) in
  print_string (show_expr e);
  [%expect {| (Lam x_0 : int . (Lam x_1 : int . (Var x_1))) |}]

let%expect_test "rename: free variable in body is unchanged" =
  let c = Alpha.create_counter () in
  let e = Alpha.rename c [] (Lam { x = "x"; a = TInt; e = Var "y" }) in
  print_string (show_expr e);
  [%expect {| (Lam x_0 : int . (Var y)) |}]

let%expect_test "rename: counter shared across subexpressions" =
  (* Pair of two lambdas — binders in left and right get consecutive counters *)
  let c = Alpha.create_counter () in
  let e = Alpha.rename c []
    (Pair { e1 = Lam { x = "x"; a = TInt; e = Var "x" };
            e2 = Lam { x = "x"; a = TInt; e = Var "x" } }) in
  print_string (show_expr e);
  (* OCaml evaluates record fields right-to-left, so e2 gets counter 0, e1 gets 1 *)
  [%expect {| (Pair (Lam x_1 : int . (Var x_1)) (Lam x_0 : int . (Var x_0))) |}]
