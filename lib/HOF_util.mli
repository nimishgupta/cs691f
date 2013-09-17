(** Parser and printer for HOF.

  The parsers and pretty-printers below use the following concrete syntax.

  {[
id    ::= first charater must be a letter or an underscore; remaining
          characters may include letters, numbers, and underscores

field ::= first charater must be a letter or an underscore; remaining
          characters may include letters, numbers, and underscores

int   ::= decimal integer literal with an optional negation

exp   ::= id
        | int
        | "true"
        | "false"
        | exp "*" exp
        | exp "+" exp
        | exp "-" exp
        | exp "==" exp
        | exp "&&" exp
        | exp "||" exp
        | "let" id "=" exp "in" exp
        | "if0" exp "then" exp "else" exp
        | "if" exp "then" exp "else" exp        
        | "lambda" "(" id1 "," ... "," idn ")" "." exp
        | exp "(" exp_1 "," ... "," exp_n ")"
        | "{" field_1 ":" exp_1 "," ... "," field_n ":" exp_n "}"
        | exp "." field
        | exp "[" field "->" val "]"
        | "empty"
        | e "::" e
        | "head" "(" e ")"
        | "tail" "(" e ")"
        | "empty?" "(" e ")"

  ]}
*)
open HOF_sugar

type result = 
  | Exp of exp
  | ParseError of string

val parse : string -> result

val parse_from_file : string -> result

val print_exp : exp -> unit

val string_of_exp : exp -> string
