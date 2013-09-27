open SystemF_syntax
open SystemF_util
exception Expected_error of exp
exception Fatal_error of exp

module TC = SystemF_tc

let rec subst (x : id) (v : exp) (exp : exp) : exp = match exp with
  | Int _ -> exp
  | Id (p, y) -> if x = y then v else exp
  | Fun (p, y, t, e) -> Fun (p, y, t, if x = y then e else subst x v e)
  | TypFun (p, y, e) -> TypFun (p, y, subst x v e)
  | App (p, e1, e2) -> App (p, subst x v e1, subst x v e2)
  | TypApp (p, e, t) -> TypApp (p, subst x v e, t)

let rec tsubst_exp (x : id) (t : typ) (exp : exp) : exp = match exp with
  | Int _ -> exp
  | Id _ -> exp
  | Fun (p, y, t', e) -> Fun (p, y, TC.tsubst x t t', tsubst_exp x t e)
  | TypFun (p, y, e) -> 
    if x = y then exp else TypFun (p, y, tsubst_exp x t e)
  | App (p, e1, e2) -> App (p, tsubst_exp x t e1, tsubst_exp x t e2)
  | TypApp (p, e, t') -> TypApp (p, tsubst_exp x t e, TC.tsubst x t t')

let rec eval (exp : exp) : exp = match exp with
  | Int _ -> exp
  | Fun _ -> exp
  | TypFun _ -> exp
  | Id _ -> raise (Fatal_error exp)
  | App (p, e1, e2) -> (match eval e1 with
    | Fun (_, x, _, e1') -> eval (subst x (eval e2) e1')
    | v1 -> raise (Fatal_error (App (p, v1, e2))))
  | TypApp (p, e, t) -> (match eval e with
    | TypFun (_, x, e') -> eval (tsubst_exp x t e')
    | v1 -> raise (Fatal_error (TypApp (p, v1, t))))

