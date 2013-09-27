type t

val to_string : t -> string

val compare : t -> t -> int

val mk : Lexing.position -> Lexing.position -> t

val print : t -> string -> unit