{
  open Parser
  exception Lexer_error of string
}

let white = [' ' '\t' '\r' '\n']+
let ident = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let digit = ['0'-'9']+

rule token = parse
  | white     { token lexbuf }
  | "fun"     { FUN }
  | "for"     { FOR }
  | "in"      { IN }
  | "do"      { DO }
  | "case"    { CASE }
  | "if"      { IF }
  | "inl"     { INL }
  | "inr"     { INR }
  | "bot"     { BOT }
  | "fst"     { FST }
  | "snd"     { SND }
  | "fix"     { FIX }
  | "let"     { LET }
  | "set"     { SET }
  | "int"     { INT_KW }
  | "V"       { JOIN }
  | "unit"    { UNIT_KW }
  | "->"      { ARROW }
  | "-"       { MINUS }
  | "*"       { STAR }
  | "+"       { PLUS }
  | "/"       { SLASH }
  | "=="      { EQEQ }
  | "<="      { LEQ }
  | "<"       { LT }
  | "="       { EQ }
  | ":"       { COLON }
  | ","       { COMMA }
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | "["       { LBRACKET }
  | "]"       { RBRACKET }
  | "{"       { LBRACE }
  | "}"       { RBRACE }
  | digit     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | ident     { IDENT (Lexing.lexeme lexbuf) }
  | eof       { EOF }
  | _         { raise (Lexer_error (Printf.sprintf "unexpected character '%s'" (Lexing.lexeme lexbuf))) }
