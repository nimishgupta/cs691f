(** Abstract syntax for System F. *)
type pos = Pos.t

type id = Identifier.t

type typ =
  | TInt (** [int] *)
  | TFun of typ * typ (** [t1 -> t2] *)
  | TId of id (** just like [Id x] *)
  | TForall of id * typ (** [forall a . t] *)

type exp =
  | Int of pos * int (** integers *)
  | LT of pos * exp * exp (** Primitive integer comparison *)
  | EQ of pos * exp * exp (** Primitive integer comparison *)
  | Id of pos * id (** [x], [y], [z], [x_], [y_], [z_], [x0], [y0], [z0], etc. *)
  | Fun of pos * id * typ * exp (** [fun (x : t) -> e] *)
  | App of pos * exp * exp (** [e1 e2] *)
  | TypFun of pos * id * exp (** [typfun a -> e] *)
  | TypApp of pos * exp * typ (** [e <t>] *)