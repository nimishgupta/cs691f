open SystemF_syntax
open SystemF_subst

exception Type_error of pos * string

module Id = Identifier
module Env = SystemF_typenv
module Util = SystemF_util


let rec is_typ_equal (t1 : typ) (t2 : typ) : bool = match (t1, t2) with
  | TInt, TInt -> true
  | TFun (t11, t12), TFun (t21, t22) -> 
    is_typ_equal t11 t21 && is_typ_equal t12 t22
  | TId x, TId y -> x = y
  | TForall (x, s), TForall (y, t) ->
    let z = Id.fresh_from x in
    is_typ_equal (rename_tid x z s) (rename_tid y z t)
  | _ -> false


let rec ok (env : Env.t) (typ : typ) : bool = match typ with
  | TInt -> true
  | TFun (t1, t2) -> ok env t1 && ok env t2
  | TId x -> Env.bound_tid x env
  | TForall (x, t) -> ok (Env.bind_tid x env) t

let rec tc (env : Env.t) (exp : exp) : typ = match exp with
  | Int _ -> TInt
  | Id (p, x) -> (match Env.lookup x env with
    | Some t -> t
    | None -> raise (Type_error (p, "free identifier " ^ Id.to_string x)))
  | Fun (p, x, t, e) -> 
    if ok env t then
      TFun (t, tc (Env.extend x t env) e)
    else
      raise (Type_error (p, "ill-formed type"))
  | App (p, e1, e2) -> (match tc env e1, tc env e2 with
    | TFun (t1, t2), t1' -> 
      if is_typ_equal t1 t1' then
        t2
      else
        raise (Type_error (p, 
          Format.sprintf "Function  of type:\n\
                           %s\nGot argument of type:\n%s" 
            (Util.string_of_typ (TFun (t1, t2)))
            (Util.string_of_typ t1')))
    | (t, _) -> raise (Type_error (p, 
        Format.sprintf "In a function application, the expected function:\n\
          %s\nHas type:\n%s"
          (Util.string_of_exp e1) (Util.string_of_typ t))))
  | TypFun (_, x, e) -> 
    TForall (x, tc (Env.bind_tid x env) e)
  | TypApp (p, e, t) -> (match tc env e, ok env t with
    | (TForall (x, t'), true) -> tsubst x t t'
    | (_, false) -> raise (Type_error (p,
      Format.sprintf "Type argument is ill-formed:\n%s" (Util.string_of_typ t)))
    | (s, _) -> raise (Type_error (p,
        Format.sprintf "Expression in type-function position:\n\
          %s\nHas type:\n%s" (Util.string_of_exp e) (Util.string_of_typ s))))

  let typecheck = tc Env.empty