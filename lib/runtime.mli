(** Ground data values — functions are OCaml-level only. *)
open Ast

module rec Value : sig
  type t =
    | VUnit
    | VInt  of int
    | VPair of t * t
    | VInl  of t
    | VInr  of t
    | VSet  of VSet.t
  val compare : t -> t -> int
end
and VSet : Set.S with type elt = Value.t

type value = Value.t

(** Bottom element of a lattice. *)
val bot : lattice -> value

(** Pointwise/union join. *)
val join : value -> value -> value

(** Singleton set. *)
val sing : value -> value

(** Arithmetic and comparison operations. *)
val arith : binop -> value -> value -> value

(** Iterate x := join x (f x) from bot until stable. *)
val fix : lattice -> (value -> value) -> value

(** Seminaive fixed-point. f is the monotone step; df x dx is its derivative.
    Iterates x' = x v dx, dx' = df(x)(dx) until dx adds nothing new. *)
val seminaive_fix : lattice -> (value -> value) -> (value -> value -> value) -> value

(** Fold-join: for x in s (a VSet) do f(x). *)
val for_set : lattice -> value -> (value -> value) -> value

(** Pretty-print a ground value. Raises Failure if called on a function. *)
val value_to_string : value -> string
