(** Syntactic sugar for the HOF language. *)

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
    (** Assume that all identifiers are distinct. *)
  | Apply of exp * exp list
  | Record of (field * exp) list
    (** Assume that all field names are distinct. *)
  | SetField of exp * field * exp
  | GetField of exp * field
  | True
  | False
  | If of exp * exp * exp
    (** Assume that the conditional evaluates to a boolean. *)
  | And of exp * exp
    (** Assume that the sub-expressions evalute to booleans. *)
  | Or of exp * exp
    (** Assume that the sub-expressions evalute to booleans. *)
  | IntEq of exp * exp
    (** Assume that the sub-expressions evaluate to integers. *)
  | Empty
  | Cons of exp * exp
  | Head of exp
    (** Assume that the sub-expression is either [Cons] or [Empty]. *)
  | Tail of exp
    (** Assume that the sub-expression is either [Cons] or [Empty]. *)
  | IsEmpty of exp
    (** Assume that the sub-expression is either [Cons] or [Empty]. *)
  
