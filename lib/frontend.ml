exception ParseError of string

let parse lexbuf =
  try Parser.prog Lexer.token lexbuf
  with
  | Lexer.Lexer_error msg ->
    raise (ParseError ("Lexer error: " ^ msg))
  | Parser.Error ->
    let pos = Lexing.lexeme_start_p lexbuf in
    raise (ParseError (Printf.sprintf "Parse error at line %d, column %d"
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)))

let parse_string s   = parse (Lexing.from_string s)
let parse_channel ic = parse (Lexing.from_channel ic)
