open Ast
open Type_checker

(* Convert AST types from OCaml types to strings representing those types (for the runtime) *)
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

(* Compile a Datafun expression to an OCaml expression string.
   ctx is the typing context, used to recover types where the AST omits them. *)
let compile_op = function
  | Add -> "Add" | Sub -> "Sub" | Mul -> "Mul" | Div -> "Div"
  | Eq  -> "Eq"  | Lt  -> "Lt"  | Le  -> "Le"

let rec compile (ctx : ctx) : expr -> string = function
  | Var x -> x

  | Lit n -> Printf.sprintf "(VInt %d)" n

  | BinOp { op; e1; e2 } ->
    Printf.sprintf "(arith %s (%s) (%s))"
      (compile_op op) (compile ctx e1) (compile ctx e2)

  | Unit -> "VUnit"

  | Lam { x; a; e } ->
    Printf.sprintf "(VFunc (fun %s -> %s))"
      x (compile (extend x Ord a ctx) e)

  | App { e1; e2 } ->
    Printf.sprintf "apply (%s) (%s)"
      (compile ctx e1) (compile ctx e2)

  | Pair { e1; e2 } ->
    Printf.sprintf "(VPair (%s, %s))" (compile ctx e1) (compile ctx e2)

  | ProjL e ->
    Printf.sprintf "(projl (%s))"
      (compile ctx e)

  | ProjR e ->
    Printf.sprintf "(projr (%s))"
      (compile ctx e)

  | Inl { e; _ } -> Printf.sprintf "(VInl (%s))" (compile ctx e)

  | Inr { e; _ } -> Printf.sprintf "(VInr (%s))" (compile ctx e)

  | Case { e; x; e1; y; e2 } ->
    (* Synthesize scrutinee type to know what x and y are bound to. *)
    let t1, t2 = match synth ctx e with
      | TSum (a, b) -> (a, b)
      | _ -> failwith "codegen: case expects sum type"
    in
    Printf.sprintf "(case (%s) (VFunc (fun %s -> %s)) (VFunc (fun %s -> %s)))"
      (compile ctx e) x (compile (extend x Ord t1 ctx) e1) y (compile (extend y Ord t2 ctx) e2)

  | Bot l ->
    Printf.sprintf "(bot (%s))" (compile_lattice l)

  | Join { e1; e2} ->
    Printf.sprintf "(join (%s) (%s))" (compile ctx e1) (compile ctx e2)

  | Sing e ->
    Printf.sprintf "(sing (%s))" (compile ctx e)

  | For { e1; x; e2 } ->
    (* The AST doesn't carry the body lattice, so we recover it via synth. *)
    let t = match synth ctx e2 with
      | TPow t -> t
      | _ -> failwith "codegen: for set expects power type"
    in
    let ctx' = extend x Disc (fin_to_typ t) ctx in
    let l = match typ_to_lattice (synth ctx' e1) with
      | Some l -> l
      | None   -> failwith "codegen: for body must be a lattice type"
    in
    Printf.sprintf "(for_set (%s) (%s) (VFunc (fun %s -> %s)))"
      (compile_lattice l) (compile ctx e2) x (compile ctx' e1)

  | Box e -> compile (restrict ctx) e

  | LetBox { x; e1; e2 } ->
    let a = match synth ctx e1 with
      | TBox a -> a
      | _ -> failwith "codegen: let[] expects box type"
    in
    Printf.sprintf "apply (VFunc (fun %s -> (%s))) (%s)"
      x (compile (extend x Disc a ctx) e2) (compile ctx e1)

  | Fix { x; l; e } ->
    Printf.sprintf "(fix (%s) (VFunc (fun %s -> %s)))"
      (compile_lattice l) x
      (compile (extend x Ord (lattice_to_typ l) (restrict ctx)) e)



let header = "open Datafun_lib\nopen Ast\nopen Runtime\nopen Value\n\n"

let compile_program (e : expr) : string =
  typecheck e;
  header
  ^ "let result =\n  " ^ compile empty e ^ "\n\n"
  ^ "let () = print_endline (value_to_string result)\n"
