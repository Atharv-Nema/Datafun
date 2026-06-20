open Datafun_lib
open Pretty

let parse s =
  match Frontend.parse_string s with
  | Ok e    -> show_expr e
  | Error m -> "Error: " ^ m

(* --- Atoms --- *)

let%expect_test "parse: int literal" =
  print_string (parse "5");
  [%expect {| (Lit 5) |}]

let%expect_test "parse: variable" =
  print_string (parse "x");
  [%expect {| (Var x) |}]

let%expect_test "parse: unit" =
  print_string (parse "()");
  [%expect {| Unit |}]

let%expect_test "parse: pair" =
  print_string (parse "(1, 2)");
  [%expect {| (Pair (Lit 1) (Lit 2)) |}]

let%expect_test "parse: fst" =
  print_string (parse "fst (1, 2)");
  [%expect {| (ProjL (Pair (Lit 1) (Lit 2))) |}]

let%expect_test "parse: snd" =
  print_string (parse "snd (1, 2)");
  [%expect {| (ProjR (Pair (Lit 1) (Lit 2))) |}]

let%expect_test "parse: inl" =
  print_string (parse "(inl 5 : int + unit)");
  [%expect {| (Inl (Lit 5) : (int + unit)) |}]

let%expect_test "parse: inr" =
  print_string (parse "(inr () : int + unit)");
  [%expect {| (Inr Unit : (int + unit)) |}]

let%expect_test "parse: singleton" =
  print_string (parse "{5}");
  [%expect {| (Sing (Lit 5)) |}]

let%expect_test "parse: box" =
  print_string (parse "[x]");
  [%expect {| (Box (Var x)) |}]

let%expect_test "parse: bot" =
  print_string (parse "(bot : unit)");
  [%expect {| (Bot unit) |}]

(* --- Arithmetic precedence and associativity --- *)

let%expect_test "parse: mul binds tighter than add" =
  print_string (parse "1 + 2 * 3");
  [%expect {| (BinOp Add (Lit 1) (BinOp Mul (Lit 2) (Lit 3))) |}]

let%expect_test "parse: subtraction is left-associative" =
  print_string (parse "1 - 2 - 3");
  [%expect {| (BinOp Sub (BinOp Sub (Lit 1) (Lit 2)) (Lit 3)) |}]

let%expect_test "parse: equality" =
  print_string (parse "1 == 2");
  [%expect {| (BinOp Eq (Lit 1) (Lit 2)) |}]

let%expect_test "parse: less-than-or-equal" =
  print_string (parse "1 <= 2");
  [%expect {| (BinOp Le (Lit 1) (Lit 2)) |}]

let%expect_test "parse: less-than" =
  print_string (parse "1 < 2");
  [%expect {| (BinOp Lt (Lit 1) (Lit 2)) |}]

(* --- Compound expressions --- *)

let%expect_test "parse: lambda" =
  print_string (parse "fun (x : int) -> x");
  [%expect {| (Lam x : int . (Var x)) |}]

let%expect_test "parse: application" =
  print_string (parse "f x");
  [%expect {| (App (Var f) (Var x)) |}]

let%expect_test "parse: application is left-associative" =
  print_string (parse "f x y");
  [%expect {| (App (App (Var f) (Var x)) (Var y)) |}]

let%expect_test "parse: join" =
  print_string (parse "x V y");
  [%expect {| (Join (Var x) (Var y)) |}]

let%expect_test "parse: for" =
  print_string (parse "for x in s do x");
  [%expect {| (For x in (Var s) do (Var x)) |}]

let%expect_test "parse: fix" =
  print_string (parse "fix (x : unit) -> x");
  [%expect {| (Fix x : unit . (Var x)) |}]

let%expect_test "parse: let-box" =
  print_string (parse "let [x] = e1 in e2");
  [%expect {| (LetBox x (Var e1) (Var e2)) |}]

let%expect_test "parse: case" =
  print_string (parse "case (e, inl (x) -> e1, inr (y) -> e2)");
  [%expect {| (Case (Var e) (inl x -> (Var e1)) (inr y -> (Var e2))) |}]

(* --- Error cases --- *)

let%expect_test "parse error: lambda missing type annotation" =
  print_string (parse "fun x -> x");
  [%expect {| Error: Parse error at line 1, column 5 |}]

let%expect_test "parse error: empty input" =
  print_string (parse "");
  [%expect {| Error: Parse error at line 1, column 1 |}]

let%expect_test "parse error: incomplete expression" =
  print_string (parse "1 +");
  [%expect {| Error: Parse error at line 1, column 4 |}]
