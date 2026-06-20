open Datafun_lib
open Ast
open Type_checker
open Pretty
open Alpha

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

(* --- alpha_rename --- *)
(* Alpha-rename resets the counter each call; we check structural shape not names. *)

let is_fresh_of base name =
  (* fresh names have the form "base_N" for some non-negative integer N *)
  let prefix = base ^ "_" in
  String.length name > String.length prefix
  && String.sub name 0 (String.length prefix) = prefix

let%expect_test "alpha_rename: variable not renamed" =
  (* Free variables are not renamed *)
  let e = alpha_rename (Var "x") in
  print_string (show_expr e);
  [%expect {| (Var x) |}]

let%expect_test "alpha_rename: lambda binder is freshened" =
  let e = alpha_rename (Lam { x = "x"; a = TInt; e = Var "x" }) in
  (match e with
   | Lam { x; e = Var y; _ } ->
     Printf.printf "binder_fresh=%b same_in_body=%b"
       (is_fresh_of "x" x) (x = y)
   | _ -> print_string "unexpected shape");
  [%expect {| binder_fresh=true same_in_body=true |}]

let%expect_test "alpha_rename: shadowing — inner binder gets distinct name" =
  (* fun (x:int) -> fun (x:int) -> x  — inner x should shadow outer *)
  let e = alpha_rename
    (Lam { x = "x"; a = TInt;
           e = Lam { x = "x"; a = TInt; e = Var "x" } }) in
  (match e with
   | Lam { x = x1; e = Lam { x = x2; e = Var y; _ }; _ } ->
     Printf.printf "outer_fresh=%b inner_fresh=%b distinct=%b body_matches_inner=%b"
       (is_fresh_of "x" x1) (is_fresh_of "x" x2) (x1 <> x2) (y = x2)
   | _ -> print_string "unexpected shape");
  [%expect {| outer_fresh=true inner_fresh=true distinct=true body_matches_inner=true |}]

let%expect_test "alpha_rename: free variable in lambda body is unchanged" =
  (* fun (x:int) -> y  — y is free, must not be renamed *)
  let e = alpha_rename (Lam { x = "x"; a = TInt; e = Var "y" }) in
  (match e with
   | Lam { e = Var y; _ } -> print_string y
   | _ -> print_string "unexpected shape");
  [%expect {| y |}]
