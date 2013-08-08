type pos = Lexing.position

type id = string

type exp =
  | Int of int
  | Add of exp * exp
  | Mul of exp * exp
  | Let of id * exp * exp
  | Id of id