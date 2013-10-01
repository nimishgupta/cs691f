type pos = Pos.t
open SystemF_subst
module F = SystemF_syntax

type id = Identifier.t

type typ =
  | TInt
  | TFun of typ * typ
  | TId of id
  | TForall of id * typ
  | TAbbrv of id * typ list

type exp =
  | Int of pos * int
  | LT of pos * exp * exp (** Primitive integer comparison. *)
  | EQ of pos * exp * exp (** Primitive integer comparison. *)  
  | Id of pos * id
  | Fun of pos * id * typ * exp
  | App of pos * exp * exp
  | TypFun of pos * id * exp
  | TypApp of pos * exp * typ

type typAbbrv = TypAbbrv of id list * typ

type cmd =
  | NewType of id * typAbbrv
  | NamedExp of id * exp
  | Exp of exp

type decls = (id * typAbbrv) list

let rec desugar_typ (decls : decls) (typ : typ) : F.typ = match typ with
  | TInt -> F.TInt
  | TFun (t1, t2) -> F.TFun (desugar_typ decls t1, desugar_typ decls t2)
  | TId x -> F.TId x
  | TForall (x, t) -> F.TForall (x, desugar_typ decls t)
  | TAbbrv (x, ts) -> (match List.assoc x decls with
    | TypAbbrv (xs, t) ->
        List.fold_right2 
          tsubst 
          xs (List.map (desugar_typ decls) ts)
          (desugar_typ decls t))

let rec desugar_exp (decls : decls) (exp : exp) = match exp with
  | Int (p, n) -> F.Int (p, n)
  | LT (p, e1, e2) -> F.LT (p, desugar_exp decls e1, desugar_exp decls e2)
  | EQ (p, e1, e2) -> F.EQ (p, desugar_exp decls e1, desugar_exp decls e2)
  | Id (p, x) -> F.Id (p, x)
  | Fun (p, x, t, e) -> F.Fun (p, x, desugar_typ decls t, desugar_exp decls e)
  | App (p, e1, e2) -> F.App (p, desugar_exp decls e1, desugar_exp decls e2)
  | TypFun (p, x, e) -> F.TypFun (p, x, desugar_exp decls e)
  | TypApp (x, e, t) -> F.TypApp (x, desugar_exp decls e, desugar_typ decls t)

let rec desugar (types : decls) (subst : F.exp -> F.exp) (exp : exp) = 
  subst (desugar_exp types exp)
