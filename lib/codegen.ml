open Ast
open Type_checker

(* Post-typecheck: synth cannot fail on a well-typed expression. *)
let synth_exn ctx e =
  match synth ctx e with
  | Ok t    -> t
  | Error _ -> assert false

let rec compile_fin = function
  | FUnit          -> "FUnit"
  | FInt           -> "FInt"
  | FProd (t1, t2) -> Printf.sprintf "(FProd (%s, %s))" (compile_fin t1) (compile_fin t2)
  | FSum  (t1, t2) -> Printf.sprintf "(FSum (%s, %s))"  (compile_fin t1) (compile_fin t2)
  | FPow t         -> Printf.sprintf "(FPow (%s))" (compile_fin t)
  | FBox t         -> Printf.sprintf "(FBox (%s))" (compile_fin t)

let rec compile_lattice = function
  | LUnit          -> "LUnit"
  | LProd (l1, l2) -> Printf.sprintf "(LProd (%s, %s))" (compile_lattice l1) (compile_lattice l2)
  | LPow t         -> Printf.sprintf "(LPow (%s))" (compile_fin t)

let compile_op = function
  | Add -> "Add" | Sub -> "Sub" | Mul -> "Mul" | Div -> "Div"
  | Eq  -> "Eq"  | Lt  -> "Lt"  | Le  -> "Le"

(* Zero change of a value v of finite type ft. For-loop bodies bind d_x to
   this so nested Box sub-expressions can reference the element's change. *)
let rec zero_change_fin (ft : fin_typ) (v : string) : string =
  match ft with
  | FUnit | FInt | FBox _ -> "VUnit"
  | FPow ft' -> Printf.sprintf "(bot %s)" (compile_lattice (LPow ft'))
  | FProd (a, b) ->
    Printf.sprintf "(match (%s) with VPair (l_, r_) -> VPair (%s, %s) | _ -> assert false)"
      v (zero_change_fin a "l_") (zero_change_fin b "r_")
  | FSum (a, b) ->
    Printf.sprintf "(match (%s) with VInl l_ -> VInl (%s) | VInr r_ -> VInr (%s) | _ -> assert false)"
      v (zero_change_fin a "l_") (zero_change_fin b "r_")

(* compile (= phi): seminaive value translation.
   deriv   (= delta): seminaive change translation.
   Mutually recursive because Box in compile calls deriv. *)
let rec compile (ctx : ctx) : expr -> string = function
  | Var x -> x

  | Lit n -> Printf.sprintf "(VInt %d)" n

  | BinOp { op; e1; e2 } ->
    Printf.sprintf "(arith %s %s %s)"
      (compile_op op) (compile ctx e1) (compile ctx e2)

  | Unit -> "VUnit"

  | Lam { x; a; e } ->
    Printf.sprintf "(fun %s -> %s)"
      x (compile (extend x Ord a ctx) e)

  | App { e1; e2 } ->
    Printf.sprintf "((%s) (%s))"
      (compile ctx e1) (compile ctx e2)

  | Pair { e1; e2 } ->
    Printf.sprintf "(VPair (%s, %s))" (compile ctx e1) (compile ctx e2)

  | ProjL e ->
    Printf.sprintf "(match (%s) with VPair (l_, _) -> l_ | _ -> assert false)"
      (compile ctx e)

  | ProjR e ->
    Printf.sprintf "(match (%s) with VPair (_, r_) -> r_ | _ -> assert false)"
      (compile ctx e)

  | Inl { e; _ } -> Printf.sprintf "(VInl (%s))" (compile ctx e)

  | Inr { e; _ } -> Printf.sprintf "(VInr (%s))" (compile ctx e)

  | Case { e; x; e1; y; e2 } ->
    let t1, t2 = match synth_exn ctx e with
      | TSum (a, b) -> (a, b)
      | _ -> assert false
    in
    Printf.sprintf "(match (%s) with VInl %s -> (%s) | VInr %s -> (%s) | _ -> assert false)"
      (compile ctx e)
      x (compile (extend x Ord t1 ctx) e1)
      y (compile (extend y Ord t2 ctx) e2)

  | Bot l ->
    Printf.sprintf "(bot %s)" (compile_lattice l)

  | Join { e1; e2 } ->
    Printf.sprintf "(join %s %s)" (compile ctx e1) (compile ctx e2)

  | Sing e ->
    Printf.sprintf "(sing %s)" (compile ctx e)

  | For { e1; x; e2 } ->
    let t = match synth_exn ctx e2 with
      | TPow t -> t
      | _      -> assert false
    in
    let ctx' = extend x Disc (fin_to_typ t) ctx in
    let l = match typ_to_lattice (synth_exn ctx' e1) with
      | Some l -> l
      | None   -> assert false
    in
    Printf.sprintf "(for_set %s %s (fun %s -> let d_%s = %s in %s))"
      (compile_lattice l) (compile ctx e2) x x (zero_change_fin t x) (compile ctx' e1)

  | Box e ->
    let rctx = restrict ctx in
    Printf.sprintf "(%s, %s)" (compile rctx e) (deriv rctx e)

  | LetBox { x; e1; e2 } ->
    let a = match synth_exn ctx e1 with
      | TBox a -> a
      | _      -> assert false
    in
    let ctx' = extend x Disc a ctx in
    Printf.sprintf "(let (%s, d_%s) = %s in %s)"
      x x (compile ctx e1) (compile ctx' e2)

  | Fix { x; l; e } ->
    let ctx' = extend x Ord (lattice_to_typ l) (restrict ctx) in
    Printf.sprintf "(seminaive_fix %s (fun %s -> %s) (fun %s d_%s -> %s))"
      (compile_lattice l) x (compile ctx' e) x x (deriv ctx' e)

and deriv (ctx : ctx) : expr -> string = function
  | Var x -> "d_" ^ x

  | Lit _ | Unit -> "VUnit"

  | BinOp { op; e1; e2 } ->
    (match op with
     | Add | Sub | Mul | Div -> "VUnit"
     | Eq | Lt | Le ->
       let cv = Printf.sprintf "(arith %s %s %s)" (compile_op op) (compile ctx e1) (compile ctx e2) in
       Printf.sprintf "(match (%s) with VInl _ -> VInl VUnit | VInr _ -> VInr VUnit | _ -> assert false)"
         cv)

  | Pair { e1; e2 } ->
    Printf.sprintf "(VPair (%s, %s))" (deriv ctx e1) (deriv ctx e2)

  | ProjL e ->
    Printf.sprintf "(match (%s) with VPair (dl_, _) -> dl_ | _ -> assert false)"
      (deriv ctx e)

  | ProjR e ->
    Printf.sprintf "(match (%s) with VPair (_, dr_) -> dr_ | _ -> assert false)"
      (deriv ctx e)

  | Inl { e; _ } -> Printf.sprintf "(VInl (%s))" (deriv ctx e)

  | Inr { e; _ } -> Printf.sprintf "(VInr (%s))" (deriv ctx e)

  | Case { e; x; e1; y; e2 } ->
    let t1, t2 = match synth_exn ctx e with
      | TSum (a, b) -> (a, b)
      | _ -> assert false
    in
    Printf.sprintf
      "(match (%s, %s) with (VInl %s, VInl d_%s) -> (%s) | (VInr %s, VInr d_%s) -> (%s) | _ -> assert false)"
      (compile ctx e) (deriv ctx e)
      x x (deriv (extend x Ord t1 ctx) e1)
      y y (deriv (extend y Ord t2 ctx) e2)

  | Lam { x; a; e } ->
    Printf.sprintf "(fun %s -> fun d_%s -> %s)"
      x x (deriv (extend x Ord a ctx) e)

  | App { e1; e2 } ->
    Printf.sprintf "(((%s) (%s)) (%s))"
      (deriv ctx e1) (compile ctx e2) (deriv ctx e2)

  | Bot l ->
    Printf.sprintf "(bot %s)" (compile_lattice l)

  | Join { e1; e2 } ->
    Printf.sprintf "(join %s %s)" (deriv ctx e1) (deriv ctx e2)

  | Sing e ->
    let ft = match typ_to_fin (synth_exn ctx e) with
      | Some t -> t
      | None   -> assert false
    in
    Printf.sprintf "(bot %s)" (compile_lattice (LPow ft))

  | For { e1; x; e2 } ->
    let t = match synth_exn ctx e2 with
      | TPow t -> t
      | _      -> assert false
    in
    let ctx' = extend x Disc (fin_to_typ t) ctx in
    let l = match typ_to_lattice (synth_exn ctx' e1) with
      | Some l -> l
      | None   -> assert false
    in
    let zcf = zero_change_fin t x in
    Printf.sprintf
      "(join (for_set %s %s (fun %s -> let d_%s = %s in %s)) (for_set %s (join %s %s) (fun %s -> let d_%s = %s in %s)))"
      (compile_lattice l) (deriv ctx e2) x x zcf (compile ctx' e1)
      (compile_lattice l) (compile ctx e2) (deriv ctx e2) x x zcf (deriv ctx' e1)

  | Box _ -> "VUnit"

  | LetBox { x; e1; e2 } ->
    let a = match synth_exn ctx e1 with
      | TBox a -> a
      | _      -> assert false
    in
    let ctx' = extend x Disc a ctx in
    Printf.sprintf "(let (%s, d_%s) = %s in %s)"
      x x (compile ctx e1) (deriv ctx' e2)

  | Fix { l; _ } ->
    Printf.sprintf "(bot %s)" (compile_lattice l)


let header = "open Datafun_lib\nopen Ast\nopen Runtime\nopen Value\n\n"

let compile_program (e : expr) : (string, Type_checker.error) result =
  match Type_checker.synth empty e with
  | Error _ as err -> err
  | Ok _ ->
    Ok (header
        ^ "let result =\n  " ^ compile empty e ^ "\n\n"
        ^ "let () = print_endline (value_to_string result)\n")
