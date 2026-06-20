open Ast

(* Pretty-printers for the Datafun AST.

   Two styles are provided:
   - [show_typ]/[show_fin]/[show_lattice] render *surface syntax* (e.g.
     "int -> set int"). These are used in type-checker tests and error messages,
     where readability matters most.
   - [show_expr] renders a *structural* S-expression (e.g.
     "(BinOp Add (Var x) (Lit 5))"). This makes the exact shape of the parse
     tree explicit, so parser tests can pin down precedence and associativity.

   All output is fully determined by the input (no incidental whitespace), so it
   is safe to use as the basis for golden / snapshot tests. *)

(* ----- Types (surface syntax) ----- *)

(* Finite types T ::= 1 | int | T*T | T+T | set T | [T] *)
let rec show_fin = function
  | FUnit          -> "unit"
  | FInt           -> "int"
  | FProd (t1, t2) -> Printf.sprintf "(%s * %s)" (show_fin t1) (show_fin t2)
  | FSum  (t1, t2) -> Printf.sprintf "(%s + %s)" (show_fin t1) (show_fin t2)
  | FPow  t        -> Printf.sprintf "set %s" (show_fin t)
  | FBox  t        -> Printf.sprintf "[%s]" (show_fin t)

(* Lattice types L ::= 1 | L*L | set T *)
let rec show_lattice = function
  | LUnit          -> "unit"
  | LProd (l1, l2) -> Printf.sprintf "(%s * %s)" (show_lattice l1) (show_lattice l2)
  | LPow  t        -> Printf.sprintf "set %s" (show_fin t)

(* General types A ::= 1 | int | A*B | A->B | A+B | set T | [A] *)
let rec show_typ = function
  | TUnit          -> "unit"
  | TInt           -> "int"
  | TProd (a, b)   -> Printf.sprintf "(%s * %s)" (show_typ a) (show_typ b)
  | TFun  (a, b)   -> Printf.sprintf "(%s -> %s)" (show_typ a) (show_typ b)
  | TSum  (a, b)   -> Printf.sprintf "(%s + %s)" (show_typ a) (show_typ b)
  | TPow  t        -> Printf.sprintf "set %s" (show_fin t)
  | TBox  a        -> Printf.sprintf "[%s]" (show_typ a)

(* ----- Operators ----- *)

let show_binop = function
  | Add -> "Add" | Sub -> "Sub" | Mul -> "Mul" | Div -> "Div"
  | Eq  -> "Eq"  | Lt  -> "Lt"  | Le  -> "Le"

(* ----- Expressions (structural S-expression) ----- *)

let rec show_expr = function
  | Var x                  -> Printf.sprintf "(Var %s)" x
  | Lit n                  -> Printf.sprintf "(Lit %d)" n
  | Unit                   -> "Unit"
  | BinOp { op; e1; e2 }   ->
    Printf.sprintf "(BinOp %s %s %s)" (show_binop op) (show_expr e1) (show_expr e2)
  | Pair { e1; e2 }        ->
    Printf.sprintf "(Pair %s %s)" (show_expr e1) (show_expr e2)
  | ProjL e                -> Printf.sprintf "(ProjL %s)" (show_expr e)
  | ProjR e                -> Printf.sprintf "(ProjR %s)" (show_expr e)
  | Inl { e; t }           ->
    Printf.sprintf "(Inl %s : %s)" (show_expr e) (show_typ t)
  | Inr { e; t }           ->
    Printf.sprintf "(Inr %s : %s)" (show_expr e) (show_typ t)
  | Case { e; x; e1; y; e2 } ->
    Printf.sprintf "(Case %s (inl %s -> %s) (inr %s -> %s))"
      (show_expr e) x (show_expr e1) y (show_expr e2)
  | Bot l                  -> Printf.sprintf "(Bot %s)" (show_lattice l)
  | Join { e1; e2 }        ->
    Printf.sprintf "(Join %s %s)" (show_expr e1) (show_expr e2)
  (* AST stores e1 = body, e2 = set; render in surface order "for x in set do body". *)
  | For { e1; x; e2 }      ->
    Printf.sprintf "(For %s in %s do %s)" x (show_expr e2) (show_expr e1)
  | Sing e                 -> Printf.sprintf "(Sing %s)" (show_expr e)
  | Box e                  -> Printf.sprintf "(Box %s)" (show_expr e)
  | LetBox { x; e1; e2 }   ->
    Printf.sprintf "(LetBox %s %s %s)" x (show_expr e1) (show_expr e2)
  | Fix { x; l; e }        ->
    Printf.sprintf "(Fix %s : %s . %s)" x (show_lattice l) (show_expr e)
  | Lam { x; a; e }        ->
    Printf.sprintf "(Lam %s : %s . %s)" x (show_typ a) (show_expr e)
  | App { e1; e2 }         ->
    Printf.sprintf "(App %s %s)" (show_expr e1) (show_expr e2)
