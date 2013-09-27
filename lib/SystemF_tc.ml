open SystemF_syntax

exception Type_error of pos * string

module Id = Identifier
module Env = SystemF_typenv
module Util = SystemF_util

let rec is_free_tid (x : id) (t : typ) : bool = match t with
  | TInt -> false
  | TFun (t1, t2) -> is_free_tid x t1 || is_free_tid x t2
  | TId y -> x = y
  | TForall (y, t) -> if x = y then false else is_free_tid x t

let rec rename_tid (x : id) (fresh_x : id) (t : typ) : typ = match t with
  | TInt -> TInt
  | TFun (t1, t2) -> TFun (rename_tid x fresh_x t1, rename_tid x fresh_x t2)
  | TId y -> if x = y then TId fresh_x else TId y
  | TForall (y, t) ->
    if x = y then TForall (y, t) else TForall (y, rename_tid x fresh_x t)

let rec is_typ_equal (t1 : typ) (t2 : typ) : bool = match (t1, t2) with
  | TInt, TInt -> true
  | TFun (t11, t12), TFun (t21, t22) -> 
    is_typ_equal t11 t21 && is_typ_equal t12 t22
  | TId x, TId y -> x = y
  | TForall (x, s), TForall (y, t) ->
    let z = Id.fresh_from x in
    is_typ_equal (rename_tid x z s) (rename_tid y z t)
  | _ -> false

let rec tsubst (x : id) (u : typ) (t : typ) : typ = match t with
  | TInt -> TInt
  | TFun (t1, t2) -> TFun (tsubst x u t1, tsubst x u t2)
  | TId y -> if x = y then u else TId y
  | TForall (y, t) ->
    if x = y then TForall (y, t) 
    else if is_free_tid y u then
      let y' = Id.fresh_from y in
      TForall (y', tsubst x u (rename_tid y y' t))
    else 
      TForall (y, tsubst x u t)

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