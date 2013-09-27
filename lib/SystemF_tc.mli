open SystemF_syntax

exception Type_error of pos * string

val tsubst : id -> typ -> typ -> typ

val typecheck : exp -> typ