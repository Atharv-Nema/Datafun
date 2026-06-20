open Datafun_lib

let die fmt = Printf.ksprintf (fun s -> Printf.eprintf "%s\n" s; exit 1) fmt

let () =
  let n = Array.length Sys.argv in
  if n < 2 then die "Usage: datafun <program.df> [-o output]";
  let df_file = Sys.argv.(1) in
  let exe =
    if n >= 4 && Sys.argv.(2) = "-o" then Sys.argv.(3)
    else Filename.remove_extension (Filename.basename df_file)
  in

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

  (* argv.(0) is the absolute path to this binary; lib sits two levels up then "lib" *)
  let lib = Filename.concat
    (Filename.dirname (Filename.dirname Sys.argv.(0))) "lib" in
  let cmi = lib ^ "/.datafun_lib.objs/byte" in
  let cmd = Printf.sprintf
    "ocamlopt -w -8 -I %s -I %s %s/datafun_lib.cmxa %s -o %s"
    (Filename.quote lib) (Filename.quote cmi)
    (Filename.quote lib)
    (Filename.quote tmp) (Filename.quote exe)
  in
  let ret = Sys.command cmd in
  Sys.remove tmp;
  if ret <> 0 then die "Internal error: generated code failed to compile";
  Printf.printf "Compiled: %s\n" exe
