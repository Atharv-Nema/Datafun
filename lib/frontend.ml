let parse lexbuf =
  match Parser.prog Lexer.token lexbuf with
  | exception Lexer.Lexer_error msg ->
    Error ("Lexer error: " ^ msg)
  | exception Parser.Error ->
    let pos = Lexing.lexeme_start_p lexbuf in
    Error (Printf.sprintf "Parse error at line %d, column %d"
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1))
  | e -> Ok e

let parse_string s   = parse (Lexing.from_string s)
let parse_channel ic = parse (Lexing.from_channel ic)
