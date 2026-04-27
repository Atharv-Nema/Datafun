open Ast

(* Functions are OCaml-level; value covers only ground/set-element types. *)

module rec Value : sig
  type t =
    | VUnit
    | VInt  of int
    | VPair of t * t
    | VInl  of t
    | VInr  of t
    | VSet  of VSet.t
    | VFunc of (Value.t -> Value.t)
  val compare : t -> t -> int
end = struct
  type t =
    | VUnit
    | VInt  of int
    | VPair of t * t
    | VInl  of t
    | VInr  of t
    | VSet  of VSet.t
    | VFunc of (Value.t -> Value.t)
  let rec compare a b =
    match a, b with
    | VUnit,          VUnit          -> 0
    | VInt a,         VInt b         -> Int.compare a b
    | VPair (a1, a2), VPair (b1, b2) ->
      let c = compare a1 b1 in if c <> 0 then c else compare a2 b2
    | VInl a,         VInl b         -> compare a b
    | VInr a,         VInr b         -> compare a b
    | VInl _,         VInr _         -> -1
    | VInr _,         VInl _         ->  1
    | VSet sa,        VSet sb        -> VSet.compare sa sb
    | _ -> failwith "invalid comparison"
end
and VSet : Set.S with type elt = Value.t = Set.Make(Value)
type value = Value.t
open Value

let rec bot : lattice -> value = function
  | LUnit          -> VUnit
  | LProd (l1, l2) -> VPair (bot l1, bot l2)
  | LPow _         -> VSet VSet.empty

let rec join (v1 : value) (v2 : value) : value =
  match v1, v2 with
  | VUnit,          VUnit          -> VUnit
  | VPair (a1, b1), VPair (a2, b2) -> VPair (join a1 a2, join b1 b2)
  | VSet s1,        VSet s2        -> VSet (VSet.union s1 s2)
  | _ -> failwith "Runtime.join: type mismatch"

let sing (v : value) : value = VSet (VSet.singleton v)

let bool_val b = if b then VInr VUnit else VInl VUnit

let arith op (VInt a) (VInt b) = match op with
  | Add -> VInt (a + b)
  | Sub -> VInt (a - b)
  | Mul -> VInt (a * b)
  | Div -> VInt (a / b)
  | Eq  -> bool_val (a = b)
  | Lt  -> bool_val (a < b)
  | Le  -> bool_val (a <= b)

(* Expressions are compiled down to sets and functions *)
let for_set (l: lattice) (VSet v) (VFunc f): value =
  VSet.fold (fun x acc -> join acc (f x)) v (bot l)
  

let apply (VFunc f) v = f v

let projl (VPair (x, _)) = x

let projr (VPair (_, x)) = x

let case e (VFunc l) (VFunc r) = match e with
| VInl x -> l x
| VInr y -> r y
| _ -> failwith "invalid case"

(* Iterate x := x ∨ f(x) from ⊥_l until stable. *)
let fix (l : lattice) (VFunc f) : value =
  let rec loop x =
    let x' = join x (f x) in
    if Value.compare x x' = 0 then x else loop x'
  in
  loop (bot l)

let rec value_to_string = function
  | VUnit          -> "()"
  | VInt n         -> string_of_int n
  | VPair (v1, v2) -> Printf.sprintf "(%s, %s)" (value_to_string v1) (value_to_string v2)
  | VInl v         -> Printf.sprintf "inl(%s)" (value_to_string v)
  | VInr v         -> Printf.sprintf "inr(%s)" (value_to_string v)
  | VSet s         ->
      let elts = VSet.elements s in
      let content = String.concat ", " (List.map value_to_string elts) in
      "{" ^ content ^ "}"
  | VFunc _ -> failwith "toplevel should not evaluate to a function"

