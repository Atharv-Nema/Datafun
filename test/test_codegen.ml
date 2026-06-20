open Datafun_lib
open Ast
open Type_checker
open Codegen

(* Compile an expression to its OCaml string representation. *)
let cg ctx e = compile ctx e

(* Compile a top-level program and return the full generated source. *)
let prog s =
  match Frontend.parse_string s with
  | Error m -> "Parse error: " ^ m
  | Ok e ->
    let e' = Alpha.alpha_rename e in
    match compile_program e' with
    | Error err -> "Type error: " ^ show_error err
    | Ok src -> src

(* --- atoms --- *)

let%expect_test "cg: variable" =
  print_string (cg [] (Var "x"));
  [%expect {| x |}]

let%expect_test "cg: int literal" =
  print_string (cg [] (Lit 42));
  [%expect {| (VInt 42) |}]

let%expect_test "cg: unit" =
  print_string (cg [] Unit);
  [%expect {| VUnit |}]

(* --- arithmetic --- *)

let%expect_test "cg: add" =
  print_string (cg [] (BinOp { op = Add; e1 = Lit 1; e2 = Lit 2 }));
  [%expect {| (arith Add (VInt 1) (VInt 2)) |}]

let%expect_test "cg: add nested" =
  print_string (cg [] (BinOp { op = Add; e1 = BinOp { op = Mul; e1 = Lit 2; e2 = Lit 3 }; e2 = Lit 1 }));
  [%expect {| (arith Add (arith Mul (VInt 2) (VInt 3)) (VInt 1)) |}]

let%expect_test "cg: equality" =
  print_string (cg [] (BinOp { op = Eq; e1 = Lit 1; e2 = Lit 2 }));
  [%expect {| (arith Eq (VInt 1) (VInt 2)) |}]

(* --- lambda and application --- *)

let%expect_test "cg: lambda" =
  print_string (cg [] (Lam { x = "x"; a = TInt; e = Var "x" }));
  [%expect {| (fun x -> x) |}]

let%expect_test "cg: application" =
  print_string (cg [] (App { e1 = Var "f"; e2 = Var "x" }));
  [%expect {| ((f) (x)) |}]

(* --- product --- *)

let%expect_test "cg: pair" =
  print_string (cg [] (Pair { e1 = Lit 1; e2 = Unit }));
  [%expect {| (VPair ((VInt 1), VUnit)) |}]

let%expect_test "cg: fst" =
  print_string (cg [] (ProjL (Var "p")));
  [%expect {| (match (p) with VPair (l_, _) -> l_ | _ -> assert false) |}]

let%expect_test "cg: snd" =
  print_string (cg [] (ProjR (Var "p")));
  [%expect {| (match (p) with VPair (_, r_) -> r_ | _ -> assert false) |}]

(* --- sum --- *)

let%expect_test "cg: inl" =
  print_string (cg [] (Inl { e = Lit 1; t = TSum (TInt, TUnit) }));
  [%expect {| (VInl ((VInt 1))) |}]

let%expect_test "cg: inr" =
  print_string (cg [] (Inr { e = Unit; t = TSum (TInt, TUnit) }));
  [%expect {| (VInr (VUnit)) |}]

let%expect_test "cg: case" =
  let e = Case {
    e  = Inl { e = Lit 1; t = TSum (TInt, TUnit) };
    x  = "a"; e1 = Lit 0;
    y  = "b"; e2 = Lit 1 } in
  print_string (cg [] e);
  [%expect {|
    (match ((VInl ((VInt 1)))) with VInl a -> ((VInt 0)) | VInr b -> ((VInt 1)) | _ -> assert false) |}]

(* --- lattice --- *)

let%expect_test "cg: bot unit" =
  print_string (cg [] (Bot LUnit));
  [%expect {| (bot LUnit) |}]

let%expect_test "cg: bot set" =
  print_string (cg [] (Bot (LPow FInt)));
  [%expect {| (bot (LPow (FInt))) |}]

let%expect_test "cg: join" =
  print_string (cg [] (Join { e1 = Bot LUnit; e2 = Bot LUnit }));
  [%expect {| (join (bot LUnit) (bot LUnit)) |}]

let%expect_test "cg: singleton" =
  print_string (cg [("x", Disc, TInt)] (Sing (Var "x")));
  [%expect {| (sing x) |}]

(* --- box / let-box --- *)

let%expect_test "cg: box erases" =
  (* Box just restricts the context; the expression itself is unchanged *)
  print_string (cg [("x", Disc, TInt)] (Box (Var "x")));
  [%expect {| x |}]

let%expect_test "cg: let-box becomes let" =
  (* Box (Lit 1) erases to (VInt 1); LetBox becomes let x = ... in ... *)
  let e = LetBox { x = "x"; e1 = Box (Lit 1); e2 = Var "x" } in
  print_string (cg [] e);
  [%expect {| (let x = (VInt 1) in x) |}]

(* --- for --- *)

let%expect_test "cg: for" =
  let e = For { e2 = Var "s"; x = "x"; e1 = Sing (Var "x") } in
  print_string (cg [("s", Ord, TPow FInt); ("x", Disc, TInt)] e);
  [%expect {| (for_set (LPow (FInt)) s (fun x -> (sing x))) |}]

(* --- fix --- *)

let%expect_test "cg: fix" =
  let e = Fix { x = "x"; l = LUnit; e = Unit } in
  print_string (cg [] e);
  [%expect {| (fix LUnit (fun x -> VUnit)) |}]

let%expect_test "cg: fix over set" =
  let e = Fix { x = "acc"; l = LPow FInt; e = Sing (Var "acc") } in
  print_string (cg [("acc", Ord, TPow FInt)] e);
  [%expect {| (fix (LPow (FInt)) (fun acc -> (sing acc))) |}]

(* --- full program --- *)

let%expect_test "prog: integer literal" =
  print_string (prog "5");
  [%expect {|
    open Datafun_lib
    open Ast
    open Runtime
    open Value

    let result =
      (VInt 5)

    let () = print_endline (value_to_string result) |}]

let%expect_test "prog: singleton set" =
  print_string (prog "{5}");
  [%expect {|
    open Datafun_lib
    open Ast
    open Runtime
    open Value

    let result =
      (sing (VInt 5))

    let () = print_endline (value_to_string result) |}]

let%expect_test "prog: type error is caught" =
  print_string (prog "1 + ()");
  [%expect {| Type error: arithmetic operands must both have type int |}]
