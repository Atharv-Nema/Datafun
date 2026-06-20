open Datafun_lib

let die fmt = Printf.ksprintf (fun s -> Printf.eprintf "%s\n" s; exit 1) fmt

let () =
  if Array.length Sys.argv < 2 then
    die "Usage: datafun <program.df>";
  let df_file = Sys.argv.(1) in

  let ic = open_in df_file in
  let expr =
    Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      match Frontend.parse_channel ic with
      | Ok e    -> e
      | Error s -> die "%s" s)
  in

  let expr = Alpha.alpha_rename expr in
  let src =
    match Codegen.compile_program expr with
    | Ok s      -> s
    | Error err -> die "Type error: %s" (Type_checker.show_error err)
  in

  let tmp = Filename.temp_file "datafun_" ".ml" in
  (let oc = open_out tmp in output_string oc src; close_out oc);

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
