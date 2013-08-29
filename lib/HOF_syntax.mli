(** Abstract syntax for the HOF language. *)

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
