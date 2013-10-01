(** Abstract syntax for System F macro language. *)
type pos = Pos.t
type id = Identifier.t

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

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
  | Abbrv of pos * id * (exp, typ) either list (** [X], [Y], etc. *)

type abbrv =
  | TypAbbrv of id list * typ (** type [[t x y z]] = ... *)
  | ExpAbbrv of (id, id) either list * exp (** expression [[C x <T> y]] = ... *)

type decls = (id * abbrv) list

type cmd =
  | NewAbbrv of id * abbrv
  | Exp of exp


val desugar : decls -> exp -> SystemF_syntax.exp
