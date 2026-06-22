let project_root =
  let rec go dir =
    if Sys.file_exists (Filename.concat dir "dune-project") then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then failwith "dune-project not found"
      else go parent
  in
  go (Sys.getcwd ())

let main_exe = Filename.concat project_root "_build/default/bin/main.exe"

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  String.trim (Bytes.to_string s)

(* Write source to a temp .df file, run the compiler, run the output binary.
   Returns the program's stdout, or the compiler's stderr on failure. *)
let run source =
  let df_file  = Filename.temp_file "datafun_e2e_" ".df"  in
  let exe_file = Filename.temp_file "datafun_exe_" ""      in
  let out_file = exe_file ^ ".out" in
  let err_file = exe_file ^ ".err" in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun f -> try Sys.remove f with _ -> ())
        [df_file; exe_file; out_file; err_file])
    (fun () ->
      let oc = open_out df_file in
      output_string oc source;
      close_out oc;
      let compile_cmd = Printf.sprintf "%s %s -o %s > /dev/null 2> %s"
        (Filename.quote main_exe) (Filename.quote df_file)
        (Filename.quote exe_file) (Filename.quote err_file)
      in
      if Sys.command compile_cmd <> 0 then read_file err_file
      else begin
        ignore (Sys.command (Printf.sprintf "%s > %s 2>&1"
          (Filename.quote exe_file) (Filename.quote out_file)));
        read_file out_file
      end)

(* --- atoms and arithmetic --- *)

let%expect_test "e2e: integer literal" =
  print_string (run "5");
  [%expect {| 5 |}]

let%expect_test "e2e: unit" =
  print_string (run "()");
  [%expect {| () |}]

let%expect_test "e2e: addition" =
  print_string (run "1 + 2");
  [%expect {| 3 |}]

let%expect_test "e2e: precedence mul-over-add" =
  print_string (run "1 + 2 * 3");
  [%expect {| 7 |}]

let%expect_test "e2e: subtraction left-assoc" =
  print_string (run "10 - 3 - 2");
  [%expect {| 5 |}]

let%expect_test "e2e: equality true gives inr" =
  print_string (run "1 == 1");
  [%expect {| inr(()) |}]

let%expect_test "e2e: equality false gives inl" =
  print_string (run "1 == 2");
  [%expect {| inl(()) |}]

(* --- products --- *)

let%expect_test "e2e: pair" =
  print_string (run "(1, 2)");
  [%expect {| (1, 2) |}]

let%expect_test "e2e: fst" =
  print_string (run "fst (3, 4)");
  [%expect {| 3 |}]

let%expect_test "e2e: snd" =
  print_string (run "snd (3, 4)");
  [%expect {| 4 |}]

(* --- sums --- *)

let%expect_test "e2e: case inl branch" =
  print_string (run "case ((inl 5 : int + unit), inl (x) -> x, inr (y) -> 0)");
  [%expect {| 5 |}]

let%expect_test "e2e: case inr branch" =
  print_string (run "case ((inr () : int + unit), inl (x) -> x, inr (y) -> 0)");
  [%expect {| 0 |}]

(* --- functions --- *)

let%expect_test "e2e: identity function" =
  print_string (run "(fun (x : int) -> x) 42");
  [%expect {| 42 |}]

let%expect_test "e2e: function with arithmetic" =
  print_string (run "(fun (x : int) -> x + 1) 5");
  [%expect {| 6 |}]

let%expect_test "e2e: higher-order function" =
  print_string (run "(fun (f : int -> int) -> f 3) (fun (x : int) -> x + 1)");
  [%expect {| 4 |}]

(* --- sets --- *)

let%expect_test "e2e: singleton" =
  print_string (run "{5}");
  [%expect {| {5} |}]

let%expect_test "e2e: join two singletons" =
  print_string (run "{1} V {2}");
  [%expect {| {1, 2} |}]

let%expect_test "e2e: bot set" =
  print_string (run "(bot : set int)");
  [%expect {| {} |}]

(* --- for comprehension --- *)

let%expect_test "e2e: for passthrough" =
  print_string (run "for x in {1} do {x}");
  [%expect {| {1} |}]

let%expect_test "e2e: for maps elements" =
  print_string (run "for x in ({1} V {2} V {3}) do {x}");
  [%expect {| {1, 2, 3} |}]

(* --- fix --- *)

let%expect_test "e2e: fix unit converges immediately" =
  print_string (run "fix (x : unit) -> ()");
  [%expect {| () |}]

let%expect_test "e2e: fix accumulates one element" =
  print_string (run "fix (x : set int) -> {1} V x");
  [%expect {| {1} |}]

let%expect_test "e2e: fix accumulates two elements" =
  print_string (run "fix (x : set int) -> {1} V {2} V x");
  [%expect {| {1, 2} |}]

(* --- box / let-box --- *)

let%expect_test "e2e: let-box" =
  print_string (run "let [x] = [5] in x");
  [%expect {| 5 |}]

let%expect_test "e2e: let-box with computation" =
  print_string (run "let [x] = [1 + 1] in x");
  [%expect {| 2 |}]

(* --- seminaive: let-box with function --- *)

let%expect_test "e2e: let-box applies boxed function" =
  print_string (run "let [f] = [fun (x : int) -> x + 1] in f 5");
  [%expect {| 6 |}]

let%expect_test "e2e: nested let-box" =
  print_string (run "let [x] = [3] in let [y] = [x + 1] in x + y");
  [%expect {| 7 |}]

let%expect_test "e2e: fix with seminaive converges correctly" =
  (* Adds {0}, then for each element x rebinds it via let [n] = [x] and adds {n+1} if n < 3.
     Exercises LetBox inside a for loop inside fix; converges to {0,1,2,3}. *)
  print_string (run "fix (s : set int) -> {0} V (for x in s do (let [n] = [x] in case (n < 3, inl (u) -> (bot : set int), inr (u) -> {n + 1})))");
  [%expect {| {0, 1, 2, 3} |}]

(* --- error cases --- *)

let%expect_test "e2e: parse error" =
  print_string (run "fun x -> x");
  [%expect {| Parse error at line 1, column 5 |}]

let%expect_test "e2e: type error" =
  print_string (run "1 + ()");
  [%expect {| Type error: arithmetic operands must both have type int |}]
