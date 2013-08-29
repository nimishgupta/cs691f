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

let parse_exp_from_file (file_name : string) : result =
  try Exp (parse_exp_from_file file_name)
  with Lexparse_util.Error str -> ParseError str

let parse_exp_from_string (str : string) : result = 
  parse_from_lexbuf (Lexing.from_string str)

module Format = struct

  open Format

  (* Indicates the immediately surrounding expression, which determines whether
     or not we need parentheses. *)
  type cxt = Top | Field | AndL | AndR | OrL | OrR | CmpL | CmpR 
    | AddL | AddR | MulL | MulR  | List | Atom


  let print_paren (cxt : cxt) (e : exp) : bool = match e with
    | Cons _ -> cxt > List
    | Record _ -> false
    | Let _ -> cxt > Top
    | Mul _ -> cxt > MulL
    | Add _ -> cxt > AddL
    | Sub _ -> cxt > AddL
    | GetField _ -> cxt > Field
    | SetField _ -> cxt > Field
    | _ -> false

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
        fprintf fmt "@[%a +@ %a@]" (exp AddL) e1 (exp AddR) e2
    | Sub (e1, e2) ->
        fprintf fmt "@[%a -@ %a@]" (exp AddL) e1 (exp AddR) e2
    | Mul (e1, e2) ->
        fprintf fmt "@[%a *@ %a@]" (exp MulL) e1 (exp MulR) e2
    | Let (x, e1, e2) ->
      fprintf fmt "@[<v>let @[%s =@;<1 2>%a in@]@ %a@]" x 
        (exp Top) e1 (exp Top) e2        
    | If0 (e1, e2, e3) ->
        fprintf fmt "@[ifzero %a@;<1 2>@[then@;<1 2>%a@]@;<1 2>@[else@;<1 2>%a@]@]"
          (exp Top) e1 (exp Top) e2 (exp Top) e3
    | Lambda (xs, e) ->
      fprintf fmt "@[<hv 2>lambda(%a) .@ %a" id_list xs (exp Top) e
    | Apply (fn, args) ->
      fprintf fmt "@[%a(%a)@]" (exp Field) fn exp_list args
    | GetField (obj, fld) ->
      fprintf fmt "@[%a.%s@]" (exp Field) obj fld
    | SetField (obj, fld, value) ->
      fprintf fmt "@[%a[@[%s ->@;<1 2>%a@]]@]" 
      (exp Field) obj fld (exp Top) value
    | Record flds ->
      fprintf fmt "@[<hv>{%a@ }@]" fields flds
    | If (e1, e2, e3) ->
        fprintf fmt "@[if %a@;<1 2>@[then@;<1 2>%a@]@;<1 2>@[else@;<1 2>%a@]@]"
          (exp Top) e1 (exp Top) e2 (exp Top) e3
    | And (e1, e2) -> fprintf fmt "@[%a &&@ %a]" (exp AndL) e1 (exp AndR) e2
    | Or (e1, e2) -> fprintf fmt "@[%a ||@ %a]" (exp OrL) e1 (exp OrR) e2
    | IntEq (e1, e2) -> fprintf fmt "@[%a ==@ %a]" (exp CmpL) e1 (exp CmpR) e2
    | Empty -> fprintf fmt "empty"
    | IsEmpty e -> fprintf fmt "@[<hv 2>empty(@,%a@,)@]" (exp Top) e
    | Cons (e1, e2) ->
      fprintf fmt "@[<hv>%a@ ::@ %a@]" (exp Atom) e1 (exp List) e2)

  and exp_list (fmt : formatter) (exps : exp list) : unit = match exps with
    | [] -> ()
    | [e] -> exp Top fmt e
    | e :: es -> fprintf fmt "@[%a,@;<1 0>%a@]" (exp Top) e exp_list es

  and fields (fmt : formatter) (flds : (field * exp) list) : unit =
    match flds with
    | [] -> ()
    | [(x,v)] -> fprintf fmt "@;<1 2>@[%s:@ %a@]" x (exp Top) v
    | (x,v) :: rest -> 
      fprintf fmt "@;<1 2>@[%s:@ %a,@]%a" x (exp Top) v fields rest

end

let format_exp = Format.exp Format.Top

let string_of_exp (e : exp) : string = make_string_of format_exp e

let print_exp (e : exp) : unit = print_string (string_of_exp e)
