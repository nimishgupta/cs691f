(** Abstract syntax for System F macro language. *)
type pos = Pos.t
type id = Identifier.t

type typ =
  | TInt (** [int] *)
  | TFun of typ * typ (** [t1 -> t2] *)
  | TId of id (** just like [Id x] *)
  | TForall of id * typ (** [forall a . t] *)
  | TAbbrv of id * typ list

type exp =
  | Int of pos * int (** integers *)
  | Id of pos * id (** [x], [y], [z], [x_], [y_], [z_], [x0], [y0], [z0], etc. *)
  | Fun of pos * id * typ * exp (** [fun (x : t) -> e] *)
  | App of pos * exp * exp (** [e1 e2] *)
  | TypFun of pos * id * exp (** [typfun a -> e] *)
  | TypApp of pos * exp * typ (** [e <t>] *)

type typAbbrv = TypAbbrv of id list * typ

type cmd =
  | NewType of id * typAbbrv
  | NamedExp of id * exp
  | Exp of exp

type decls = (id * typAbbrv) list

val desugar : decls -> (SystemF_syntax.exp -> SystemF_syntax.exp) -> exp -> SystemF_syntax.exp
