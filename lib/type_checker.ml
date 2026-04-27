open Ast

type qualifier = Disc | Ord

type ctx = (var * qualifier * typ) list

let empty : ctx = []

let extend x q a (ctx : ctx) : ctx = (x, q, a) :: ctx

let restrict (ctx : ctx) : ctx =
  List.filter (fun (_, q, _) -> q = Disc) ctx

let lookup x (ctx : ctx) =
  match List.find_opt (fun (v, _, _) -> v = x) ctx with
  | Some (_, _, a) -> Some a
  | None -> None

exception TypeError of string

let fail fmt = Printf.ksprintf (fun s -> raise (TypeError s)) fmt

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
  | TUnit          -> Some FUnit
  | TInt           -> Some FInt
  | TProd (a, b)   ->
    (match typ_to_fin a, typ_to_fin b with
     | Some t1, Some t2 -> Some (FProd (t1, t2))
     | _                -> None)
  | TSum (a, b)    ->
    (match typ_to_fin a, typ_to_fin b with
     | Some t1, Some t2 -> Some (FSum (t1, t2))
     | _                -> None)
  | TPow t         -> Some (FPow t)
  | TBox a         ->
    (match typ_to_fin a with
     | Some t -> Some (FBox t)
     | None   -> None)
  | TFun _         -> None

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
    (* Var / DVar *)
    (match lookup x ctx with
     | Some a -> a
     | None   -> fail "unbound variable: %s" x)

  | Lit _ -> TInt

  | BinOp { op; e1; e2 } ->
    (match synth ctx e1, synth ctx e2 with
     | TInt, TInt ->
       (match op with
        | Add | Sub | Mul | Div -> TInt
        | Eq | Lt | Le -> TSum (TUnit, TUnit))
     | _ -> fail "arithmetic: operands must have type int")

  | Unit ->
    (* 1I *)
    TUnit

  | Lam { x; a; e } ->
    (* λI: Γ,x:A ⊢ e:B  →  Γ ⊢ λx:A.e : A→B *)
    let b = synth (extend x Ord a ctx) e in
    TFun (a, b)

  | App { e1; e2 } ->
    (* λE: Γ ⊢ e1:A→B  Γ ⊢ e2:A  →  Γ ⊢ e1 e2 : B *)
    (match synth ctx e1 with
     | TFun (a, b) ->
       let a' = synth ctx e2 in
       if a = a' then b
       else fail "application: argument type mismatch"
     | _ -> fail "application: expected a function type")

  | Pair { e1; e2 } ->
    (* ×I *)
    TProd (synth ctx e1, synth ctx e2)

  | ProjL e ->
    (* ×E left *)
    (match synth ctx e with
     | TProd (a, _) -> a
     | _ -> fail "π₁: expected a product type")

  | ProjR e ->
    (* ×E right *)
    (match synth ctx e with
     | TProd (_, b) -> b
     | _ -> fail "π₂: expected a product type")

  | Inl { e; t } ->
    (* +I left: annotation t must be A1+A2, check e:A1 *)
    (match t with
     | TSum (a1, _) ->
       let a = synth ctx e in
       if a = a1 then t
       else fail "inl: expression type does not match annotation"
     | _ -> fail "inl: annotation must be a sum type")

  | Inr { e; t } ->
    (* +I right: annotation t must be A1+A2, check e:A2 *)
    (match t with
     | TSum (_, a2) ->
       let a = synth ctx e in
       if a = a2 then t
       else fail "inr: expression type does not match annotation"
     | _ -> fail "inr: annotation must be a sum type")

  | Case { e; x; e1; y; e2 } ->
    (* +E: Γ ⊢ e:A1+A2  Γ,x:A1 ⊢ e1:C  Γ,y:A2 ⊢ e2:C *)
    (match synth ctx e with
     | TSum (a1, a2) ->
       let c1 = synth (extend x Ord a1 ctx) e1 in
       let c2 = synth (extend y Ord a2 ctx) e2 in
       if c1 = c2 then c1
       else fail "case: branches have incompatible types"
     | _ -> fail "case: scrutinee must have a sum type")

  | Box e ->
    (* DI: [Γ] ⊢ e:A  →  Γ ⊢ [e]:[A] *)
    let a = synth (restrict ctx) e in
    TBox a

  | LetBox { x; e1; e2 } ->
    (* DE: Γ ⊢ e1:[A]  Γ,x:ᴰA ⊢ e2:C  →  Γ ⊢ let[x]=e1 in e2 : C *)
    (match synth ctx e1 with
     | TBox a -> synth (extend x Disc a ctx) e2
     | _      -> fail "let[]: expected a box type")

  | Bot l ->
    (* T⊥: Γ ⊢ ⊥_L : L *)
    lattice_to_typ l

  | Sing e ->
    (* Tone: [Γ] ⊢ e:T  →  Γ ⊢ {e}:𝒫(T) *)
    let a = synth (restrict ctx) e in
    (match typ_to_fin a with
     | Some t -> TPow t
     | None   -> fail "singleton: element type must be a finite type")

  | Join { e1; e2 } ->
    (* T∨: Γ ⊢ e1:L  Γ ⊢ e2:L  →  Γ ⊢ e1 ∨ e2 : L *)
    let t1 = synth ctx e1 and t2 = synth ctx e2 in
    if t1 = t2 then t1
    else fail "join: operands must have the same lattice type"

  | For { e1; x; e2 } ->
    (* Tchoose: Γ ⊢ e2:𝒫(T)  Γ,x:ᴰT ⊢ e1:L  →  Γ ⊢ for(e1).x∈e2 : L *)
    (* In AST: e1=body (x bound here), e2=set *)
    (match synth ctx e2 with
     | TPow t ->
       let lt = synth (extend x Disc (fin_to_typ t) ctx) e1 in
       (match typ_to_lattice lt with
        | Some _ -> lt
        | None   -> fail "for: body must have a lattice type")
     | _ -> fail "for: set expression must have type 𝒫(T)")

  | Fix { x; l; e } ->
    (* Tfix: [Γ],x:L ⊢ e:L  →  Γ ⊢ fix x:L.e : L *)
    let lt = lattice_to_typ l in
    let t = synth (extend x Ord lt (restrict ctx)) e in
    if t = lt then lt
    else fail "fix: body type must match the declared lattice type"

let typecheck e =
  match synth [] e with
  | TFun _ -> raise (TypeError "top-level expression cannot have a function type")
  | _ -> ()