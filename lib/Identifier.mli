type t

val fresh : string -> t

val to_string : t -> string

val fresh_from : t -> t

val compare : t -> t -> int

val from_string : string -> t