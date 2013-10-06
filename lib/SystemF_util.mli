open SystemF_syntax

type result = 
  | Command of SystemF_sugar.cmd list
  | ParseError of string 

val parse_from_file : string -> result

val parse : string -> result
