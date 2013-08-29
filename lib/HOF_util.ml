open HOF_syntax
open PL_util

module Parsers = Lexparse_util.MakeParsers (struct
  exception ParseError = HOF_parser.Error
  type token = HOF_parser.token
  type exp = HOF_syntax.exp
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
  type cxt = Top | AddL | AddR | MulL | MulR | Field

  let print_paren (cxt : cxt) (e : exp) : bool = match e with
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
        fprintf fmt "@[if %a@;<1 2>@[then@;<1 2>%a@]@;<1 2>@[else@;<1 2>%a@]@]"
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
      fprintf fmt "@[<hv>{%a@ }@]" fields flds)

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

  let rec value (fmt : formatter) (value : value) : unit = match value with
    | IntVal n -> fprintf fmt "%d" n
    | ClosureVal (env_, xs, e) ->
      fprintf fmt "@[<%a> lambda(%a) . %a@]" env env_ id_list xs (exp Top) e

  and env (fmt : formatter) (binds : (id * value) list) : unit =
    match binds with
      | [] -> ()
      | [(x,v)] -> fprintf fmt "@[%s = %a]" x value v
      | (x,v) :: binds' -> fprintf fmt "@[%s = %a;@;<1 0>%a]" x value v env binds'

end

let format_exp = Format.exp Format.Top

let format_value = Format.value

let string_of_exp (e : exp) : string = make_string_of format_exp e

let print_exp (e : exp) : unit = print_string (string_of_exp e)

let string_of_value (v : value) : string = make_string_of format_value v

let print_value (v : value) : unit = print_string (string_of_value v)