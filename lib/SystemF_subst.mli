(** Substitution functions for System F. *)
open SystemF_syntax

val subst : id -> exp -> exp -> exp

val tsubst : id -> typ -> typ -> typ

val tsubst_exp : id -> typ -> exp -> exp

(** [rename_tid x y t] renames free occurrences of [x] to [y]. [y] must be
    fresh. *)
val rename_tid : id -> id -> typ -> typ