open SystemF_syntax
open SystemF_util
open SystemF_subst
exception Expected_error of exp
exception Fatal_error of exp

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

