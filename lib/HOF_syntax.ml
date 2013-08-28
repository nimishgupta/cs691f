type id = string

type exp =
  | Int of int
  | Add of exp * exp
  | Sub of exp * exp
  | Mul of exp * exp
  | Let of id * exp * exp
  | Id of id
  | If0 of exp * exp * exp
  | Lambda of id list * exp
  | Apply of exp * exp list

type value =
  | IntVal of int
  | ClosureVal of (id * value) list * id list * exp

