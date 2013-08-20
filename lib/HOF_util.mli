(** Parser and printer for HOF. *)
open HOF_syntax

type result = 
  | Exp of exp
  | ParseError of string

val parse_exp_from_string : string -> result

val parse_exp_from_file : string -> result

val print_exp : exp -> unit

val string_of_exp : exp -> string

val print_value : value -> unit

val string_of_value : value -> string