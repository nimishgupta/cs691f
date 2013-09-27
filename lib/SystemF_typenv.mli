open SystemF_syntax

type t
 
val empty : t
val extend : id -> typ -> t -> t
val lookup : id -> t -> typ option
val bind_tid : id -> t -> t
val bound_tid : id -> t -> bool