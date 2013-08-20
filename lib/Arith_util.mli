(** Parser and printer for arithmetic expressions. *)
open Arith_syntax

type result = 
  | Exp of exp
  | ParseError of string

val parse_exp_from_string : string -> result

val parse_exp_from_file : string -> result

val print_exp : exp -> unit

val string_of_exp : exp -> string