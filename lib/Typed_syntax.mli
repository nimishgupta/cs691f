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
  | TNum (** [num] *)
  | TBool (** [bool] *)
  | TFun of typ * typ (** [t1 -> t2] *)
  | TPair of typ * typ (** [(t1, t2)] *)
  | TList of typ (** [t list] *)

type exp =
  | Int of int (** decimal integer *)
  | Bool of bool (** either [true] or [false] *)
  | Arith of arithOp * exp * exp (** [e1 + e2], [e1 - e2], or [e1 * e2] *)
  | Cmp of intCmp * exp * exp (** [e1 < e2], [e1 > e2], or [e1 = e2] *)
  | If of exp * exp * exp (** [if e1 then e2 else e2] *)
  | Id of id (** first charater must be a letter or an underscore; remaining
                 characters may include letters, numbers, and underscores *)
  | Let of id * exp * exp (** [let x = e1 in e2] *)
  | Fun of id * typ * exp (** [fun (x : t) -> e] *)
  | Fix of id * typ * exp (** [fix (x : t) -> e] *)
  | App of exp * exp (** [e1 e2] *)
  | Empty of typ (** [empty<t>] *)
  | Cons of exp * exp (** [e1 :: e2] *)
  | Head of exp (** [head e] *)
  | Tail of exp (** [tail e] *)
  | IsEmpty of exp (** [empty? e] *)
  | Pair of exp * exp (** [(e1, e2)] *)
  | ProjL of exp (** [e.1] *)
  | ProjR of exp (** [e.2] *)
