open SystemF_syntax

type result = 
  | Command of SystemF_sugar.cmd list
  | ParseError of string 

val parse_from_file : string -> result

val parse : string -> result

val string_of_exp : exp -> string

val print_exp : exp -> unit

val string_of_typ : typ -> string

val print_typ : typ -> unit
