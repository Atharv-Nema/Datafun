open Datafun_lib
open Ast
open Runtime
open Value

(* --- bot --- *)

let%expect_test "bot: unit" =
  print_string (value_to_string (bot LUnit));
  [%expect {| () |}]

let%expect_test "bot: set" =
  print_string (value_to_string (bot (LPow FInt)));
  [%expect {| {} |}]

let%expect_test "bot: product" =
  print_string (value_to_string (bot (LProd (LUnit, LPow FInt))));
  [%expect {| ((), {}) |}]

(* --- join --- *)

let%expect_test "join: unit" =
  print_string (value_to_string (join VUnit VUnit));
  [%expect {| () |}]

let%expect_test "join: sets union" =
  let s1 = sing (VInt 1) and s2 = sing (VInt 2) in
  print_string (value_to_string (join s1 s2));
  [%expect {| {1, 2} |}]

let%expect_test "join: product is componentwise" =
  let v1 = VPair (VUnit, sing (VInt 1)) in
  let v2 = VPair (VUnit, sing (VInt 2)) in
  print_string (value_to_string (join v1 v2));
  [%expect {| ((), {1, 2}) |}]

let%expect_test "join: idempotent on set" =
  let s = sing (VInt 42) in
  print_string (value_to_string (join s s));
  [%expect {| {42} |}]

(* --- sing --- *)

let%expect_test "sing: int element" =
  print_string (value_to_string (sing (VInt 5)));
  [%expect {| {5} |}]

let%expect_test "sing: unit element" =
  print_string (value_to_string (sing VUnit));
  [%expect {| {()} |}]

(* --- arith --- *)

let%expect_test "arith: add" =
  print_string (value_to_string (arith Add (VInt 3) (VInt 4)));
  [%expect {| 7 |}]

let%expect_test "arith: sub" =
  print_string (value_to_string (arith Sub (VInt 10) (VInt 3)));
  [%expect {| 7 |}]

let%expect_test "arith: mul" =
  print_string (value_to_string (arith Mul (VInt 3) (VInt 4)));
  [%expect {| 12 |}]

let%expect_test "arith: div" =
  print_string (value_to_string (arith Div (VInt 12) (VInt 4)));
  [%expect {| 3 |}]

let%expect_test "arith: eq true" =
  print_string (value_to_string (arith Eq (VInt 2) (VInt 2)));
  [%expect {| inr(()) |}]

let%expect_test "arith: eq false" =
  print_string (value_to_string (arith Eq (VInt 1) (VInt 2)));
  [%expect {| inl(()) |}]

let%expect_test "arith: lt true" =
  print_string (value_to_string (arith Lt (VInt 1) (VInt 2)));
  [%expect {| inr(()) |}]

let%expect_test "arith: le equal" =
  print_string (value_to_string (arith Le (VInt 2) (VInt 2)));
  [%expect {| inr(()) |}]

(* --- fix --- *)

let%expect_test "fix: trivial unit" =
  print_string (value_to_string (fix LUnit (fun _ -> VUnit)));
  [%expect {| () |}]

let%expect_test "fix: accumulates one element" =
  (* f adds {1} to whatever it's given — converges at {1} *)
  print_string (value_to_string (fix (LPow FInt) (fun s -> join s (sing (VInt 1)))));
  [%expect {| {1} |}]

let%expect_test "fix: product lattice" =
  (* f adds ((), {1}) — converges at ((), {1}) *)
  print_string (value_to_string
    (fix (LProd (LUnit, LPow FInt))
       (fun v -> join v (VPair (VUnit, sing (VInt 1))))));
  [%expect {| ((), {1}) |}]

(* --- for_set --- *)

let%expect_test "for_set: empty set yields bot" =
  let empty = bot (LPow FInt) in
  print_string (value_to_string (for_set (LPow FInt) empty (fun x -> sing x)));
  [%expect {| {} |}]

let%expect_test "for_set: singleton passthrough" =
  print_string (value_to_string (for_set (LPow FInt) (sing (VInt 7)) (fun x -> sing x)));
  [%expect {| {7} |}]

let%expect_test "for_set: two elements" =
  let s = join (sing (VInt 1)) (sing (VInt 2)) in
  print_string (value_to_string (for_set (LPow FInt) s (fun x -> sing x)));
  [%expect {| {1, 2} |}]

let%expect_test "for_set: maps elements" =
  (* double each element: {1,2} -> {2,4} *)
  let s = join (sing (VInt 1)) (sing (VInt 2)) in
  print_string (value_to_string
    (for_set (LPow FInt) s (fun x -> sing (arith Mul x (VInt 2)))));
  [%expect {| {2, 4} |}]

(* --- value_to_string --- *)

let%expect_test "value_to_string: unit" =
  print_string (value_to_string VUnit);
  [%expect {| () |}]

let%expect_test "value_to_string: int" =
  print_string (value_to_string (VInt 42));
  [%expect {| 42 |}]

let%expect_test "value_to_string: pair" =
  print_string (value_to_string (VPair (VInt 1, VUnit)));
  [%expect {| (1, ()) |}]

let%expect_test "value_to_string: inl" =
  print_string (value_to_string (VInl VUnit));
  [%expect {| inl(()) |}]

let%expect_test "value_to_string: inr" =
  print_string (value_to_string (VInr (VInt 3)));
  [%expect {| inr(3) |}]

let%expect_test "value_to_string: nested set" =
  let s = join (sing (VInt 1)) (join (sing (VInt 2)) (sing (VInt 3))) in
  print_string (value_to_string s);
  [%expect {| {1, 2, 3} |}]

let%expect_test "value_to_string: empty set" =
  print_string (value_to_string (VSet VSet.empty));
  [%expect {| {} |}]
