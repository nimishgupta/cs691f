type pos = Pos.t

type id = Identifier.t

type typ =
  | TInt
  | TFun of typ * typ
  | TId of id
  | TForall of id * typ

type exp =
  | Int of pos * int
  | LT of pos * exp * exp (** Primitive integer comparison *)
  | EQ of pos * exp * exp (** Primitive integer comparison *)  
  | Id of pos * id
  | Fun of pos * id * typ * exp
  | App of pos * exp * exp
  | TypFun of pos * id * exp
  | TypApp of pos * exp * typ