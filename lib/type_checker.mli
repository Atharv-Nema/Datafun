open Ast

(** Variable qualifier: Disc for discrete (modal/box-safe), Ord for ordinary. *)
type qualifier = Disc | Ord

(** Typing context: ordered list of (variable, qualifier, type) bindings. *)
type ctx = (var * qualifier * typ) list

(** Structured type errors returned by [synth]. *)
type error =
  | UnboundVar        of var
  | ArithNonInt                                      (** binop operands not both int *)
  | AppNotFunction    of typ                         (** applying a non-function *)
  | AppArgMismatch    of { expected: typ; got: typ } (** argument type mismatch *)
  | ProjNotProduct    of typ                         (** fst/snd of a non-product *)
  | InjNotSum         of typ                         (** inl/inr annotation not a sum *)
  | InjMismatch       of { expected: typ; got: typ } (** inl/inr expr type != annotation *)
  | CaseNotSum        of typ                         (** case scrutinee not a sum *)
  | BranchMismatch    of { left: typ; right: typ }   (** case branches disagree *)
  | LetBoxNotBox      of typ                         (** let[x]=e: e not a box type *)
  | SingNotFinite     of typ                         (** {e}: e not a finite type *)
  | JoinMismatch      of { left: typ; right: typ }   (** join operands disagree *)
  | ForNotSet         of typ                         (** for x in e: e not a set *)
  | ForBodyNotLattice of typ                         (** for body not a lattice type *)
  | FixBodyMismatch   of { declared: typ; got: typ } (** fix body type != declared *)

(** Render an error as a human-readable string. *)
val show_error : error -> string

(** The empty context. *)
val empty : ctx

(** [extend x q a ctx] prepends binding x:^q a to ctx. *)
val extend : var -> qualifier -> typ -> ctx -> ctx

(** [restrict ctx] keeps only Disc-qualified bindings ([.] in the rules). *)
val restrict : ctx -> ctx

(** [fin_to_typ t] converts a finite type to its corresponding general type. *)
val fin_to_typ : fin_typ -> typ

(** [lattice_to_typ l] converts a lattice type to its corresponding general type. *)
val lattice_to_typ : lattice -> typ

(** [typ_to_fin a] returns [Some t] if [a] is a finite type, [None] otherwise. *)
val typ_to_fin : typ -> fin_typ option

(** [typ_to_lattice a] returns [Some l] if [a] is a lattice type, [None] otherwise. *)
val typ_to_lattice : typ -> lattice option

(** [synth ctx e] synthesises the type of [e] under [ctx].
    Returns [Error _] on any typing failure. *)
val synth : ctx -> expr -> (typ, error) result
