open SystemF_syntax

module Id = Identifier

let rec subst (x : id) (v : exp) (exp : exp) : exp = match exp with
  | Int _ -> exp
  | Id (p, y) -> if x = y then v else exp
  | Fun (p, y, t, e) -> Fun (p, y, t, if x = y then e else subst x v e)
  | TypFun (p, y, e) -> TypFun (p, y, subst x v e)
  | App (p, e1, e2) -> App (p, subst x v e1, subst x v e2)
  | TypApp (p, e, t) -> TypApp (p, subst x v e, t)

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

let rec tsubst_exp (x : id) (t : typ) (exp : exp) : exp = match exp with
  | Int _ -> exp
  | Id _ -> exp
  | Fun (p, y, t', e) -> Fun (p, y, tsubst x t t', tsubst_exp x t e)
  | TypFun (p, y, e) -> 
    if x = y then exp else TypFun (p, y, tsubst_exp x t e)
  | App (p, e1, e2) -> App (p, tsubst_exp x t e1, tsubst_exp x t e2)
  | TypApp (p, e, t') -> TypApp (p, tsubst_exp x t e, tsubst x t t')
