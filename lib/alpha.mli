open Ast

(** Opaque counter used to generate fresh variable names. *)
type counter

(** [create_counter ()] creates a counter initialised at 0. *)
val create_counter : unit -> counter

(** [rename c subst e] renames all bound variables in [e], using [c] to
    generate fresh names and [subst] as the current variable substitution. *)
val rename : counter -> (var * var) list -> expr -> expr

(** [alpha_rename e] renames all bound variables in [e] with fresh names.
    Equivalent to [rename (create_counter ()) [] e]. *)
val alpha_rename : expr -> expr
