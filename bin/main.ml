open Datafun_lib

let die fmt = Printf.ksprintf (fun s -> Printf.eprintf "%s\n" s; exit 1) fmt

let () =
  if Array.length Sys.argv < 2 then
    die "Usage: datafun <program.df>";
  let df_file = Sys.argv.(1) in

  (* Parse *)
  let ic = open_in df_file in
  let expr =
    Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      try Frontend.parse_channel ic
      with Frontend.ParseError s -> die "%s" s)
  in

  (* Alpha-rename and compile to OCaml source *)
  let expr = Alpha.alpha_rename expr in
  let src =
    try Codegen.compile_program expr
    with Type_checker.TypeError s -> die "Type error: %s" s
  in

  (* Write generated source to a temp file *)
  let tmp = Filename.temp_file "datafun_" ".ml" in
  (let oc = open_out tmp in output_string oc src; close_out oc);

  (* Compile against the library in the dune build tree *)
  let lib = "_build/default/lib" in
  let cmi = lib ^ "/.datafun_lib.objs/byte" in
  let exe = Filename.remove_extension (Filename.basename df_file) in
  let cmd = Printf.sprintf
    "ocamlopt -w -8 -I %s -I %s %s/datafun_lib.cmxa %s -o %s"
    (Filename.quote lib) (Filename.quote cmi)
    (Filename.quote lib)
    (Filename.quote tmp) (Filename.quote exe)
  in
  let ret = Sys.command cmd in
  Sys.remove tmp;
  if ret <> 0 then die "Internal error: generated code failed to compile";
  Printf.printf "Compiled: ./%s\n" exe
