open SystemF_syntax
open PL_util

module Id = Identifier

module FormatTyp = struct

  open Format

  type cxt = ATOM | FUN | TYP

  let print_paren (cxt : cxt) (t : typ) : bool = match t with
    | TInt -> false
    | TId _ -> false
    | TFun _ -> cxt < FUN
    | TForall _ -> cxt < TYP

  let rec typ (cxt : cxt) (fmt : formatter) (t : typ) : unit =
    parens (print_paren cxt t) fmt (fun () ->
      match t with
        | TId x -> fprintf fmt "%s" (Id.to_string x)
        | TInt -> fprintf fmt "int"
        | TFun (t1, t2) ->
          fprintf fmt "@[<hv 2>%a@ -> %a@]" (typ ATOM) t1 (typ FUN) t2
        | TForall (x, t) ->
          fprintf fmt "@[<hv 2>forall %s.@ %a@]" (Id.to_string x) (typ TYP) t)

end

let format_typ = FormatTyp.typ FormatTyp.TYP

module Format = struct

  open Format


  type cxt = ATOM | APP | CMP | EXP 

  let print_paren (cxt : cxt) (e : exp) : bool = match e with
    | Int _ -> false
    | Id _ -> false
    | TypFun _ -> cxt < EXP
    | Fun _ -> cxt < EXP
    | App _ -> cxt < APP
    | LT _ -> cxt < CMP
    | EQ _ -> cxt < CMP
    | TypApp _ -> cxt < APP

  let rec exp (cxt : cxt) (fmt : formatter) (e : exp) : unit =
    parens (print_paren cxt e) fmt (fun () ->
        match e with
        | Int (_, n) -> fprintf fmt "@[%d@]" n
        | LT (_, e1, e2) ->
                  fprintf fmt "@[%a <@ %a@]" (exp APP) e1 (exp APP) e2
        | EQ (_, e1, e2) ->
                  fprintf fmt "@[%a =@ %a@]" (exp APP) e1 (exp APP) e2
        | Id (_, x) -> fprintf fmt "@[%s@]" (Id.to_string x)
        | Fun (_, x, t, e) ->
          fprintf fmt "@[<hv 2>fun (%s : %a) ->@ %a" (Id.to_string x) 
            format_typ t (exp EXP) e
        | TypFun (_, x, e) ->
          fprintf fmt "@[<hv 2>typfun %s ->@ %a" (Id.to_string x) (exp EXP) e
        | App (_, e1, e2) ->
          fprintf fmt "@[%a@ %a@]" (exp APP) e1 (exp ATOM) e2
        | TypApp (_, e1, t2) ->
          fprintf fmt "@[%a@ <%a>@]" (exp APP) e1 format_typ t2)

end

let format_exp = Format.exp Format.EXP

let string_of_exp (e : exp) : string = make_string_of format_exp e

let print_exp (e : exp) : unit = print_string (string_of_exp e)

let string_of_typ (t : typ) : string = make_string_of format_typ t

let print_typ (t : typ) : unit = print_string (string_of_typ t)

