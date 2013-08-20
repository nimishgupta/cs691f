type id = string

type op =
  | ADD
  | MUL
  | SUB
  | EQ
  | LT 

type exp =
  | Id of id
  | Int of int
  | True
  | False
  | Op2 of op * exp * exp
  | If of exp * exp * exp
  | Lambda of id list * exp
  | Apply of exp * exp list
  | Rec of id * exp