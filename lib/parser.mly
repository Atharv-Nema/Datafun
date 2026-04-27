%{
  open Ast
  open Type_checker

  let fin_exn t =
    match typ_to_fin t with
    | Some ft -> ft
    | None -> raise (TypeError "expected a finite type here")

  let lat_exn t =
    match typ_to_lattice t with
    | Some l -> l
    | None -> raise (TypeError "expected a lattice type here")
%}

%token <string> IDENT
%token <int> INT
%token FUN FOR IN DO CASE IF INL INR BOT FST SND FIX LET JOIN
%token UNIT_KW INT_KW ARROW STAR PLUS MINUS SLASH EQ EQEQ LT LEQ COLON COMMA SET
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token EOF

%start <Ast.expr> prog

%%

prog: e = expr EOF { e }

(* Types *)

typ:
  | t = typ_sum ARROW u = typ   { TFun (t, u) }
  | t = typ_sum                  { t }

typ_sum:
  | t = typ_prod PLUS u = typ_sum  { TSum (t, u) }
  | t = typ_prod                    { t }

typ_prod:
  | t = typ_atom STAR u = typ_prod  { TProd (t, u) }
  | t = typ_atom                     { t }

typ_atom:
  | UNIT_KW                        { TUnit }
  | INT_KW                         { TInt }
  | LPAREN t = typ RPAREN          { t }
  | SET t = typ_atom               { TPow (fin_exn t) }
  | LBRACKET t = typ RBRACKET      { TBox t }

(* Expressions: precedence tower lowest to highest:
   expr > join_expr V > add_expr +- > mul_expr */ > app_expr > atom_expr *)

expr:
  | FUN LPAREN x = IDENT COLON a = typ RPAREN ARROW e = expr
      { Lam { x; a; e } }
  | FOR x = IDENT IN s = app_expr DO body = expr
      { For { e1 = body; x; e2 = s } }
  | FIX LPAREN x = IDENT COLON t = typ RPAREN ARROW e = expr
      { Fix { x; l = lat_exn t; e } }
  | LET LBRACKET x = IDENT RBRACKET EQ e1 = expr IN e2 = expr
      { LetBox { x; e1; e2 } }
  | e = join_expr
      { e }

join_expr:
  | e1 = join_expr JOIN e2 = cmp_expr  { Join { e1; e2 } }
  | e = cmp_expr                        { e }

cmp_expr:
  | e1 = add_expr EQEQ e2 = add_expr  { BinOp { op = Eq; e1; e2 } }
  | e1 = add_expr LT   e2 = add_expr  { BinOp { op = Lt; e1; e2 } }
  | e1 = add_expr LEQ  e2 = add_expr  { BinOp { op = Le; e1; e2 } }
  | e = add_expr                       { e }

add_expr:
  | e1 = add_expr PLUS  e2 = mul_expr  { BinOp { op = Add; e1; e2 } }
  | e1 = add_expr MINUS e2 = mul_expr  { BinOp { op = Sub; e1; e2 } }
  | e = mul_expr                        { e }

mul_expr:
  | e1 = mul_expr STAR  e2 = app_expr  { BinOp { op = Mul; e1; e2 } }
  | e1 = mul_expr SLASH e2 = app_expr  { BinOp { op = Div; e1; e2 } }
  | e = app_expr                        { e }

app_expr:
  | f = app_expr a = atom_expr  { App { e1 = f; e2 = a } }
  | e = atom_expr               { e }

atom_expr:
  | x = IDENT                                              { Var x }
  | n = INT                                                { Lit n }
  | LPAREN RPAREN                                          { Unit }
  | LPAREN e1 = expr COMMA e2 = expr RPAREN                { Pair { e1; e2 } }
  | LPAREN INL e = expr COLON t = typ RPAREN               { Inl { e; t } }
  | LPAREN INR e = expr COLON t = typ RPAREN               { Inr { e; t } }
  | LPAREN BOT COLON t = typ RPAREN                        { Bot (lat_exn t) }
  | LPAREN e = expr RPAREN                                 { e }
  | CASE LPAREN e = expr COMMA
      INL LPAREN x = IDENT RPAREN ARROW e1 = expr COMMA
      INR LPAREN y = IDENT RPAREN ARROW e2 = expr RPAREN   { Case { e; x; e1; y; e2 } }
  | IF LPAREN cond = expr COMMA e_then = expr COMMA e_else = expr RPAREN
      { Case { e = cond; x = "_"; e1 = e_else; y = "_"; e2 = e_then } }
  | FST e = atom_expr                                      { ProjL e }
  | SND e = atom_expr                                      { ProjR e }
  | LBRACE e = expr RBRACE                                 { Sing e }
  | LBRACKET e = expr RBRACKET                             { Box e }
