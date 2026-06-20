open Ast

type qualifier = Disc | Ord
type ctx = (var * qualifier * typ) list

type error =
  | UnboundVar        of var
  | ArithNonInt
  | AppNotFunction    of typ
  | AppArgMismatch    of { expected: typ; got: typ }
  | ProjNotProduct    of typ
  | InjNotSum         of typ
  | InjMismatch       of { expected: typ; got: typ }
  | CaseNotSum        of typ
  | BranchMismatch    of { left: typ; right: typ }
  | LetBoxNotBox      of typ
  | SingNotFinite     of typ
  | JoinMismatch      of { left: typ; right: typ }
  | ForNotSet         of typ
  | ForBodyNotLattice of typ
  | FixBodyMismatch   of { declared: typ; got: typ }

let show_error = function
  | UnboundVar x ->
    Printf.sprintf "unbound variable: %s" x
  | ArithNonInt ->
    "arithmetic operands must both have type int"
  | AppNotFunction t ->
    Printf.sprintf "expected a function type, got %s" (Pretty.show_typ t)
  | AppArgMismatch { expected; got } ->
    Printf.sprintf "argument type mismatch: expected %s, got %s"
      (Pretty.show_typ expected) (Pretty.show_typ got)
  | ProjNotProduct t ->
    Printf.sprintf "expected a product type, got %s" (Pretty.show_typ t)
  | InjNotSum t ->
    Printf.sprintf "injection annotation must be a sum type, got %s" (Pretty.show_typ t)
  | InjMismatch { expected; got } ->
    Printf.sprintf "injection type mismatch: expected %s, got %s"
      (Pretty.show_typ expected) (Pretty.show_typ got)
  | CaseNotSum t ->
    Printf.sprintf "case scrutinee must have a sum type, got %s" (Pretty.show_typ t)
  | BranchMismatch { left; right } ->
    Printf.sprintf "case branches have incompatible types: %s vs %s"
      (Pretty.show_typ left) (Pretty.show_typ right)
  | LetBoxNotBox t ->
    Printf.sprintf "let[]: expected a box type, got %s" (Pretty.show_typ t)
  | SingNotFinite t ->
    Printf.sprintf "singleton: element type must be finite, got %s" (Pretty.show_typ t)
  | JoinMismatch { left; right } ->
    Printf.sprintf "join: operands have different types: %s vs %s"
      (Pretty.show_typ left) (Pretty.show_typ right)
  | ForNotSet t ->
    Printf.sprintf "for: set expression must have type set T, got %s" (Pretty.show_typ t)
  | ForBodyNotLattice t ->
    Printf.sprintf "for: body must have a lattice type, got %s" (Pretty.show_typ t)
  | FixBodyMismatch { declared; got } ->
    Printf.sprintf "fix: body type %s does not match declared lattice type %s"
      (Pretty.show_typ got) (Pretty.show_typ declared)

let empty : ctx = []
let extend x q a (ctx : ctx) : ctx = (x, q, a) :: ctx
let restrict (ctx : ctx) : ctx = List.filter (fun (_, q, _) -> q = Disc) ctx

let lookup x (ctx : ctx) =
  match List.find_opt (fun (v, _, _) -> v = x) ctx with
  | Some (_, _, a) -> Some a
  | None           -> None

let ( let* ) = Result.bind

let rec lattice_to_typ = function
  | LUnit          -> TUnit
  | LProd (l1, l2) -> TProd (lattice_to_typ l1, lattice_to_typ l2)
  | LPow t         -> TPow t

let rec fin_to_typ = function
  | FUnit          -> TUnit
  | FInt           -> TInt
  | FProd (t1, t2) -> TProd (fin_to_typ t1, fin_to_typ t2)
  | FSum  (t1, t2) -> TSum  (fin_to_typ t1, fin_to_typ t2)
  | FPow t         -> TPow t
  | FBox t         -> TBox  (fin_to_typ t)

let rec typ_to_fin = function
  | TUnit        -> Some FUnit
  | TInt         -> Some FInt
  | TProd (a, b) ->
    (match typ_to_fin a, typ_to_fin b with
     | Some t1, Some t2 -> Some (FProd (t1, t2))
     | _                -> None)
  | TSum (a, b)  ->
    (match typ_to_fin a, typ_to_fin b with
     | Some t1, Some t2 -> Some (FSum (t1, t2))
     | _                -> None)
  | TPow t       -> Some (FPow t)
  | TBox a       ->
    (match typ_to_fin a with
     | Some t -> Some (FBox t)
     | None   -> None)
  | TFun _       -> None

let rec typ_to_lattice = function
  | TUnit        -> Some LUnit
  | TProd (a, b) ->
    (match typ_to_lattice a, typ_to_lattice b with
     | Some l1, Some l2 -> Some (LProd (l1, l2))
     | _                -> None)
  | TPow t       -> Some (LPow t)
  | _            -> None

let rec synth (ctx : ctx) = function
  | Var x ->
    (match lookup x ctx with
     | Some a -> Ok a
     | None   -> Error (UnboundVar x))

  | Lit _ -> Ok TInt

  | BinOp { op; e1; e2 } ->
    let* t1 = synth ctx e1 in
    let* t2 = synth ctx e2 in
    (match t1, t2 with
     | TInt, TInt ->
       Ok (match op with
           | Add | Sub | Mul | Div -> TInt
           | Eq | Lt | Le -> TSum (TUnit, TUnit))
     | _ -> Error ArithNonInt)

  | Unit -> Ok TUnit

  | Lam { x; a; e } ->
    let* b = synth (extend x Ord a ctx) e in
    Ok (TFun (a, b))

  | App { e1; e2 } ->
    let* t1 = synth ctx e1 in
    (match t1 with
     | TFun (a, b) ->
       let* a' = synth ctx e2 in
       if a = a' then Ok b
       else Error (AppArgMismatch { expected = a; got = a' })
     | _ -> Error (AppNotFunction t1))

  | Pair { e1; e2 } ->
    let* t1 = synth ctx e1 in
    let* t2 = synth ctx e2 in
    Ok (TProd (t1, t2))

  | ProjL e ->
    let* t = synth ctx e in
    (match t with
     | TProd (a, _) -> Ok a
     | _            -> Error (ProjNotProduct t))

  | ProjR e ->
    let* t = synth ctx e in
    (match t with
     | TProd (_, b) -> Ok b
     | _            -> Error (ProjNotProduct t))

  | Inl { e; t } ->
    (match t with
     | TSum (a1, _) ->
       let* a = synth ctx e in
       if a = a1 then Ok t else Error (InjMismatch { expected = a1; got = a })
     | _ -> Error (InjNotSum t))

  | Inr { e; t } ->
    (match t with
     | TSum (_, a2) ->
       let* a = synth ctx e in
       if a = a2 then Ok t else Error (InjMismatch { expected = a2; got = a })
     | _ -> Error (InjNotSum t))

  | Case { e; x; e1; y; e2 } ->
    let* t = synth ctx e in
    (match t with
     | TSum (a1, a2) ->
       let* c1 = synth (extend x Ord a1 ctx) e1 in
       let* c2 = synth (extend y Ord a2 ctx) e2 in
       if c1 = c2 then Ok c1
       else Error (BranchMismatch { left = c1; right = c2 })
     | _ -> Error (CaseNotSum t))

  | Box e ->
    let* a = synth (restrict ctx) e in
    Ok (TBox a)

  | LetBox { x; e1; e2 } ->
    let* t1 = synth ctx e1 in
    (match t1 with
     | TBox a -> synth (extend x Disc a ctx) e2
     | _      -> Error (LetBoxNotBox t1))

  | Bot l -> Ok (lattice_to_typ l)

  | Sing e ->
    let* a = synth (restrict ctx) e in
    (match typ_to_fin a with
     | Some t -> Ok (TPow t)
     | None   -> Error (SingNotFinite a))

  | Join { e1; e2 } ->
    let* t1 = synth ctx e1 in
    let* t2 = synth ctx e2 in
    if t1 = t2 then Ok t1
    else Error (JoinMismatch { left = t1; right = t2 })

  | For { e1; x; e2 } ->
    let* t2 = synth ctx e2 in
    (match t2 with
     | TPow t ->
       let* lt = synth (extend x Disc (fin_to_typ t) ctx) e1 in
       (match typ_to_lattice lt with
        | Some _ -> Ok lt
        | None   -> Error (ForBodyNotLattice lt))
     | _ -> Error (ForNotSet t2))

  | Fix { x; l; e } ->
    let lt = lattice_to_typ l in
    let* t = synth (extend x Ord lt (restrict ctx)) e in
    if t = lt then Ok lt
    else Error (FixBodyMismatch { declared = lt; got = t })
