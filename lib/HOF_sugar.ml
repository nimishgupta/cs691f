type id = string

type field = string

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
  | Record of (field * exp) list
  | SetField of exp * field * exp
  | GetField of exp * field
  | True
  | False
  | If of exp * exp * exp
  | And of exp * exp
  | Or of exp * exp
  | IntEq of exp * exp
  | Empty of exp
  | Cons of exp * exp
  | Head of exp
  | Tail of exp
