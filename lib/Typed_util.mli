open Typed_syntax

type result = 
  | Exp of exp
  | ParseError of string

val parse : string -> result

val parse_from_file : string -> result

val print_exp : exp -> unit

val string_of_exp : exp -> string

val string_of_typ : typ -> string

val print_typ : typ -> unit