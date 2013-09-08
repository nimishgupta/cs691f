open HOF_sugar
open PL_util

module Parsers = Lexparse_util.MakeParsers (struct
    exception ParseError = HOF_parser.Error
    type token = HOF_parser.token
    type exp = HOF_sugar.exp
    let parser = HOF_parser.program
    let lexer = HOF_lexer.token
  end)

open Parsers

type result = 
  | Exp of exp
  | ParseError of string 

let parse_from_lexbuf (lexbuf : Lexing.lexbuf) : result = 
  try Exp (parse_from_lexbuf lexbuf)
  with Lexparse_util.Error str -> ParseError str

let parse_from_file (file_name : string) : result =
  try Exp (parse_exp_from_file file_name)
  with Lexparse_util.Error str -> ParseError str

let parse (str : string) : result = 
  parse_from_lexbuf (Lexing.from_string str)

module Format = struct

  open Format

  type cxt = ATOM | LIST | MUL | ADD | CMP | OR_ | AND_ | EXP

  let print_paren (cxt : cxt) (e : exp) : bool = match e with
    | Int _ -> false
    | Add _ -> cxt < ADD
    | Sub _ -> cxt < ADD
    | Mul _ -> cxt < MUL
    | Let _ -> cxt < EXP    
    | Id _ -> false
    | If0 _ -> cxt < EXP
    | Lambda _ -> cxt < EXP
    | Apply _ -> false
    | Record _ -> false
    | GetField _ -> false
    | SetField _ -> false
    | True -> false
    | False -> false
    | If _ -> cxt < EXP
    | And _ -> cxt < AND_
    | Or _ -> cxt < OR_
    | IntEq _ -> cxt < CMP
    | Empty -> false
    | Cons _ -> cxt < LIST
    | Head _ -> false
    | Tail _ -> false
    | IsEmpty _ -> false

  let rec id_list (fmt : formatter) (ids : id list) = match ids with
    | [] -> ()
    | [x] -> fprintf fmt "%s" x
    | x :: xs ->fprintf fmt "[@%s, %a@]" x id_list xs

  let rec exp (cxt : cxt) (fmt : formatter) (e : exp) : unit =
    parens (print_paren cxt e) fmt (fun () ->
        match e with
        | Int n -> fprintf fmt "@[%d@]" n
        | Id x -> fprintf fmt "@[%s@]" x
        | Add (e1, e2) ->
          fprintf fmt "@[%a +@ %a@]" (exp ADD) e1 (exp MUL) e2
        | Sub (e1, e2) ->
          fprintf fmt "@[%a -@ %a@]" (exp ADD) e1 (exp MUL) e2
        | Mul (e1, e2) ->
          fprintf fmt "@[%a *@ %a@]" (exp MUL) e1 (exp LIST) e2
        | Let (x, e1, e2) ->
          fprintf fmt "@[<v>let @[%s =@;<1 2>%a in@]@ %a@]" x 
            (exp EXP) e1 (exp EXP) e2        
        | If0 (e1, e2, e3) ->
          fprintf fmt "@[ifzero %a@;<1 2>@[then@;<1 2>%a@]@;<1 2>@[else@;<1 2>%a@]@]"
            (exp EXP) e1 (exp EXP) e2 (exp EXP) e3
        | Lambda (xs, e) ->
          fprintf fmt "@[<hv 2>lambda(%a) .@ %a" id_list xs (exp EXP) e
        | Apply (fn, args) ->
          fprintf fmt "@[%a(%a)@]" (exp ATOM) fn exp_list args
        | GetField (obj, fld) ->
          fprintf fmt "@[%a.%s@]" (exp ATOM) obj fld
        | SetField (obj, fld, value) ->
          fprintf fmt "@[%a[@[%s ->@;<1 2>%a@]]@]" (exp ATOM) obj fld (exp EXP) value
        | Record flds ->
          fprintf fmt "@[<hv>{%a@ }@]" fields flds
        | If (e1, e2, e3) ->
          fprintf fmt "@[if %a@;<1 2>@[then@;<1 2>%a@]@;<1 2>@[else@;<1 2>%a@]@]"
            (exp EXP) e1 (exp EXP) e2 (exp EXP) e3
        | And (e1, e2) ->
          fprintf fmt "@[%a &&@ %a]" (exp AND_) e1 (exp OR_) e2
        | Or (e1, e2) ->
          fprintf fmt "@[%a ||@ %a]" (exp OR_) e1 (exp CMP) e2
        | IntEq (e1, e2) -> fprintf fmt "@[%a ==@ %a]" (exp ADD) e1 (exp ADD) e2
        | Empty -> fprintf fmt "empty"
        | IsEmpty e -> fprintf fmt "@[<hv 2>empty(@,%a@,)@]" (exp EXP) e
        | Head e -> fprintf fmt "@[<hv 2>head(@,%a@,)@]" (exp EXP) e
        | Tail e -> fprintf fmt "@[<hv 2>tail(@,%a@,)@]" (exp EXP) e
        | True -> fprintf fmt "true"
        | False -> fprintf fmt "false"
        | Cons (e1, e2) ->
          fprintf fmt "@[<hv>%a@ ::@ %a@]" (exp ATOM) e1 (exp LIST) e2)

  and exp_list (fmt : formatter) (exps : exp list) : unit = match exps with
    | [] -> ()
    | [e] -> exp EXP fmt e
    | e :: es -> fprintf fmt "@[%a,@;<1 0>%a@]" (exp EXP) e exp_list es

  and fields (fmt : formatter) (flds : (field * exp) list) : unit =
    match flds with
    | [] -> ()
    | [(x,v)] -> fprintf fmt "@;<1 2>@[%s:@ %a@]" x (exp EXP) v
    | (x,v) :: rest -> 
      fprintf fmt "@;<1 2>@[%s:@ %a,@]%a" x (exp EXP) v fields rest

end

let format_exp = Format.exp Format.EXP

let string_of_exp (e : exp) : string = make_string_of format_exp e

let print_exp (e : exp) : unit = print_string (string_of_exp e)
