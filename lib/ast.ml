type var = string

type fin_typ =
  | FUnit
  | FInt
  | FProd of fin_typ * fin_typ
  | FSum  of fin_typ * fin_typ
  | FPow  of fin_typ
  | FBox  of fin_typ

type lattice =
  | LUnit
  | LProd of lattice * lattice
  | LPow  of fin_typ

type typ =
  | TUnit
  | TInt
  | TProd of typ * typ
  | TFun  of typ * typ
  | TSum  of typ * typ
  | TPow  of fin_typ
  | TBox  of typ

type binop = Add | Sub | Mul | Div | Eq | Lt | Le

type expr =
  | Var    of var
  | Lit    of int
  | BinOp  of { op: binop; e1: expr; e2: expr }
  | Lam    of { x: var; a: typ; e: expr }
  | App    of { e1: expr; e2: expr }
  | Unit
  | Pair   of { e1: expr; e2: expr }
  | ProjL  of expr
  | ProjR  of expr
  | Inl    of { e: expr; t: typ }
  | Inr    of { e: expr; t: typ }
  | Case   of { e: expr; x: var; e1: expr; y: var; e2: expr }
  | Bot    of lattice
  | Join   of { e1: expr; e2: expr }
  | For    of { e1: expr; x: var; e2: expr }
  | Sing   of expr
  | Box    of expr
  | LetBox of { x: var; e1: expr; e2: expr }
  | Fix    of { x: var; l: lattice; e: expr }
