type pos = Pos.t
open SystemF_subst
module F = SystemF_syntax

type id = Identifier.t

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

type typ =
  | TInt
  | TFun of typ * typ
  | TId of id
  | TForall of id * typ
  | TAbbrv of id * typ list

type exp =
  | Int of pos * int
  | Id of pos * id
  | Fun of pos * id * typ * exp
  | App of pos * exp * exp
  | TypFun of pos * id * exp
  | TypApp of pos * exp * typ
  | Abbrv of pos * id * (exp, typ) either list

type abbrv =
  | TypAbbrv of id list * typ
  | ExpAbbrv of (id, id) either list * exp

type decls = (id * abbrv) list

type cmd =
  | NewAbbrv of id * abbrv
  | Exp of exp

let subst_either (x : (id, id) either) (v : (F.exp, F.typ) either) (e : F.exp) : F.exp =
  match (x, v) with
    | (Left x, Left v) -> subst x v e
    | (Right x, Right v) -> tsubst_exp x v e
    | _ -> failwith "expected blah"

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
          (desugar_typ decls t)
    | ExpAbbrv _ -> failwith "expeceted type abbrv")

and desugar (decls : decls) (exp : exp) = match exp with
  | Int (p, n) -> F.Int (p, n)
  | Id (p, x) -> F.Id (p, x)
  | Fun (p, x, t, e) -> F.Fun (p, x, desugar_typ decls t, desugar decls e)
  | App (p, e1, e2) -> F.App (p, desugar decls e1, desugar decls e2)
  | TypFun (p, x, e) -> F.TypFun (p, x, desugar decls e)
  | TypApp (x, e, t) -> F.TypApp (x, desugar decls e, desugar_typ decls t)
  | Abbrv (p, x, es) -> (match List.assoc x decls with
    | ExpAbbrv (xs, e) ->
      let es' = List.map (desugar_either decls) es in
      List.fold_right2 subst_either xs es' (desugar decls e)
    | TypAbbrv _ -> failwith "expected typ")

and desugar_either (decls : decls) (x : (exp, typ) either) : (F.exp, F.typ) either = 
  match x with
    | Left x -> Left (desugar decls x)
    | Right x -> Right (desugar_typ decls x)
