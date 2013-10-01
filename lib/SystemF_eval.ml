open SystemF_syntax
open SystemF_util
open SystemF_subst
exception Expected_error of exp
exception Fatal_error of exp

module Id = Identifier

let church_true p =
  let r = Id.fresh "R" in
  let x = Id.fresh "x" in
  let y = Id.fresh "y" in
  TypFun (p, r, Fun (p, x, TId r, Fun (p, y, TId r, Id (p, x))))

let church_false p =
  let r = Id.fresh "R" in
  let x = Id.fresh "x" in
  let y = Id.fresh "y" in
  TypFun (p, r, Fun (p, x, TId r, Fun (p, y, TId r, Id (p, y))))

let rec eval (exp : exp) : exp = match exp with
  | Int _ -> exp
  | Fun _ -> exp
  | TypFun _ -> exp
  | Id _ -> raise (Fatal_error exp)
  | LT (p, e1, e2) -> (match eval e1, eval e2 with
    | Int (_, n1), Int (_, n2) -> if n1 < n2 then church_true p else church_false p
    | (v1, v2) -> raise (Fatal_error (LT (p, v1, v2))))
  | EQ (p, e1, e2) -> (match eval e1, eval e2 with
    | Int (_, n1), Int (_, n2) -> if n1 = n2 then church_true p else church_false p
    | (v1, v2) -> raise (Fatal_error (LT (p, v1, v2))))
  | App (p, e1, e2) -> (match eval e1 with
    | Fun (_, x, _, e1') -> eval (subst x (eval e2) e1')
    | v1 -> raise (Fatal_error (App (p, v1, e2))))
  | TypApp (p, e, t) -> (match eval e with
    | TypFun (_, x, e') -> eval (tsubst_exp x t e')
    | v1 -> raise (Fatal_error (TypApp (p, v1, t))))

