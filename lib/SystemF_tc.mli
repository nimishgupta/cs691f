open SystemF_syntax

exception Type_error of pos * string

val typecheck : exp -> typ