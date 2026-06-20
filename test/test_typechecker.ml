open Datafun_lib
open Ast
open Type_checker
open Pretty

let tc ctx e =
  match synth ctx e with
  | Ok t    -> show_typ t
  | Error e -> "Error: " ^ show_error e

(* --- synth: success cases --- *)

let%expect_test "synth: unit literal" =
  print_string (tc [] Unit);
  [%expect {| unit |}]

let%expect_test "synth: int literal" =
  print_string (tc [] (Lit 5));
  [%expect {| int |}]

let%expect_test "synth: Ord variable" =
  print_string (tc [("x", Ord, TInt)] (Var "x"));
  [%expect {| int |}]

let%expect_test "synth: Disc variable (DVar)" =
  print_string (tc [("x", Disc, TInt)] (Var "x"));
  [%expect {| int |}]

let%expect_test "synth: arithmetic add" =
  print_string (tc [] (BinOp { op = Add; e1 = Lit 1; e2 = Lit 2 }));
  [%expect {| int |}]

let%expect_test "synth: equality returns sum" =
  print_string (tc [] (BinOp { op = Eq; e1 = Lit 1; e2 = Lit 2 }));
  [%expect {| (unit + unit) |}]

let%expect_test "synth: pair" =
  print_string (tc [] (Pair { e1 = Lit 1; e2 = Unit }));
  [%expect {| (int * unit) |}]

let%expect_test "synth: fst" =
  print_string (tc [] (ProjL (Pair { e1 = Lit 1; e2 = Unit })));
  [%expect {| int |}]

let%expect_test "synth: snd" =
  print_string (tc [] (ProjR (Pair { e1 = Lit 1; e2 = Unit })));
  [%expect {| unit |}]

let%expect_test "synth: inl" =
  print_string (tc [] (Inl { e = Lit 1; t = TSum (TInt, TUnit) }));
  [%expect {| (int + unit) |}]

let%expect_test "synth: inr" =
  print_string (tc [] (Inr { e = Unit; t = TSum (TInt, TUnit) }));
  [%expect {| (int + unit) |}]

let%expect_test "synth: case" =
  let e = Case { e  = Inl { e = Lit 1; t = TSum (TInt, TUnit) };
                 x  = "a"; e1 = Lit 0;
                 y  = "b"; e2 = Lit 1 } in
  print_string (tc [] e);
  [%expect {| int |}]

let%expect_test "synth: lambda" =
  print_string (tc [] (Lam { x = "x"; a = TInt; e = Var "x" }));
  [%expect {| (int -> int) |}]

let%expect_test "synth: application" =
  let e = App { e1 = Lam { x = "x"; a = TInt; e = Var "x" }; e2 = Lit 3 } in
  print_string (tc [] e);
  [%expect {| int |}]

let%expect_test "synth: bot" =
  print_string (tc [] (Bot LUnit));
  [%expect {| unit |}]

let%expect_test "synth: bot set" =
  print_string (tc [] (Bot (LPow FInt)));
  [%expect {| set int |}]

let%expect_test "synth: join" =
  print_string (tc [] (Join { e1 = Bot LUnit; e2 = Bot LUnit }));
  [%expect {| unit |}]

let%expect_test "synth: singleton with Disc var" =
  print_string (tc [("x", Disc, TInt)] (Sing (Var "x")));
  [%expect {| set int |}]

let%expect_test "synth: for" =
  let e = For { e2 = Bot (LPow FInt); x = "x";
                e1 = Sing (Var "x") } in
  print_string (tc [("x", Disc, TInt)] e);
  [%expect {| set int |}]

let%expect_test "synth: box with Disc var survives restriction" =
  print_string (tc [("x", Disc, TInt)] (Box (Var "x")));
  [%expect {| [int] |}]

let%expect_test "synth: let-box binds as Disc" =
  let e = LetBox { x = "x"; e1 = Box (Lit 1); e2 = Var "x" } in
  print_string (tc [] e);
  [%expect {| int |}]

let%expect_test "synth: fix" =
  let e = Fix { x = "x"; l = LUnit; e = Unit } in
  print_string (tc [] e);
  [%expect {| unit |}]

(* --- synth: error cases --- pattern-match on the variant --- *)

let%expect_test "synth error: unbound variable" =
  print_string (tc [] (Var "x"));
  [%expect {| Error: unbound variable: x |}]

let%expect_test "synth error: arithmetic on non-int" =
  print_string (tc [] (BinOp { op = Add; e1 = Unit; e2 = Lit 1 }));
  [%expect {| Error: arithmetic operands must both have type int |}]

let%expect_test "synth error: apply non-function" =
  print_string (tc [] (App { e1 = Lit 1; e2 = Lit 2 }));
  [%expect {| Error: expected a function type, got int |}]

let%expect_test "synth error: argument type mismatch" =
  let e = App { e1 = Lam { x = "x"; a = TInt; e = Var "x" }; e2 = Unit } in
  print_string (tc [] e);
  [%expect {| Error: argument type mismatch: expected int, got unit |}]

let%expect_test "synth error: fst of non-product" =
  print_string (tc [] (ProjL (Lit 1)));
  [%expect {| Error: expected a product type, got int |}]

let%expect_test "synth error: case scrutinee not a sum" =
  let e = Case { e = Lit 1; x = "a"; e1 = Lit 0; y = "b"; e2 = Lit 1 } in
  print_string (tc [] e);
  [%expect {| Error: case scrutinee must have a sum type, got int |}]

let%expect_test "synth error: case branches disagree" =
  let e = Case { e  = Inl { e = Lit 1; t = TSum (TInt, TUnit) };
                 x  = "a"; e1 = Lit 0;
                 y  = "b"; e2 = Unit } in
  print_string (tc [] e);
  [%expect {| Error: case branches have incompatible types: int vs unit |}]

let%expect_test "synth error: box with Ord var fails" =
  print_string (tc [("x", Ord, TInt)] (Box (Var "x")));
  [%expect {| Error: unbound variable: x |}]

let%expect_test "synth error: singleton with Ord var fails" =
  print_string (tc [("x", Ord, TInt)] (Sing (Var "x")));
  [%expect {| Error: unbound variable: x |}]

let%expect_test "synth error: join type mismatch" =
  print_string (tc [] (Join { e1 = Bot LUnit; e2 = Lit 1 }));
  [%expect {| Error: join: operands have different types: unit vs int |}]

let%expect_test "synth error: for over non-set" =
  let e = For { e2 = Lit 1; x = "x"; e1 = Bot LUnit } in
  print_string (tc [] e);
  [%expect {| Error: for: set expression must have type set T, got int |}]

let%expect_test "synth error: let-box with non-box" =
  let e = LetBox { x = "x"; e1 = Lit 1; e2 = Var "x" } in
  print_string (tc [] e);
  [%expect {| Error: let[]: expected a box type, got int |}]

let%expect_test "synth error: fix body type mismatch" =
  let e = Fix { x = "x"; l = LUnit; e = Lit 1 } in
  print_string (tc [] e);
  [%expect {| Error: fix: body type int does not match declared lattice type unit |}]
