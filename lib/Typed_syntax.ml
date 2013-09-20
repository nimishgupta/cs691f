(** Abstract syntax for the Typed language. *)

type id = string

type field = string

type arithOp =
  | Plus
  | Minus
  | Times

type intCmp =
  | LT
  | GT
  | EQ

type typ =
  | TNum
  | TBool
  | TFun of typ * typ
  | TPair of typ * typ
  | TList of typ

type exp =
  | Int of int
  | Bool of bool
  | Arith of arithOp * exp * exp
  | Cmp of intCmp * exp * exp
  | If of exp * exp * exp
  | Id of id
  | Let of id * exp * exp
  | Fun of id * typ * exp
  | Fix of id * typ * exp  
  | App of exp * exp
  | Empty of typ
  | Cons of exp * exp
  | Head of exp
  | Tail of exp
  | IsEmpty of exp
  | Pair of exp * exp
  | ProjL of exp
  | ProjR of exp
