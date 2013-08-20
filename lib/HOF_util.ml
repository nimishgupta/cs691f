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
  type cxt = Top | AddL | AddR | MulL | MulR | EqLt | Fun

  let rec id_list (fmt : formatter) (ids : id list) = match ids with
    | [] -> ()
    | [x] -> fprintf fmt "%s" x
    | x :: xs ->fprintf fmt "[@%s, %a@]" x id_list xs

  let rec exp (cxt : cxt) (fmt : formatter) (e : exp) : unit = match e with
    | Int n -> fprintf fmt "@[%d@]" n
    | Id x -> fprintf fmt "@[%s@]" x
    | True -> pp_print_string fmt "true"
    | False -> pp_print_string fmt "false"  
    | Op2 (ADD, e1, e2) -> begin match cxt with
      | Fun | AddR ->
        fprintf fmt "@[(@[%a + %a@])@]" (exp AddL) e1 (exp AddR) e2
      | _ ->
        fprintf fmt "@[%a + %a@]" (exp AddL) e1 (exp AddR) e2
      end
    | Op2 (SUB, e1, e2) -> begin match cxt with
      | Fun | AddR ->
        fprintf fmt "@[(@[%a - %a@])@]" (exp AddL) e1 (exp AddR) e2
      | _ ->
        fprintf fmt "@[%a - %a@]" (exp AddL) e1 (exp AddR) e2
      end
    | Op2 (MUL, e1, e2) -> begin match cxt with
      | Top | MulL | EqLt ->
        fprintf fmt "@[%a * %a@]" (exp MulL) e1 (exp MulR) e2
      | _ ->
        fprintf fmt "@[(@[%a * %a@])@]" (exp MulL) e1 (exp MulR) e2
      end
    | Op2 (EQ, e1, e2) -> begin match cxt with
      | Top -> fprintf fmt "@[%a = %a@]" (exp EqLt) e1 (exp EqLt) e2
      | _ -> fprintf fmt "@[(@[%a = %a@])@]" (exp EqLt) e1 (exp EqLt) e2
      end
    | Op2 (LT, e1, e2) -> begin match cxt with
      | Top -> fprintf fmt "@[%a < %a@]" (exp EqLt) e1 (exp EqLt) e2
      | _ -> fprintf fmt "@[(@[%a < %a@])@]" (exp EqLt) e1 (exp EqLt) e2
      end

    | If (e1, e2, e3) -> begin match cxt with
      | Top ->
        fprintf fmt "@[if %a then %a else %a@]" (exp Top) e1 (exp Top) e2
          (exp Top) e3
      | _ -> 
        fprintf fmt "@[(@[if %a then %a else %a@])@]" (exp Top) e1 (exp Top) e2
          (exp Top) e3
      end
    | Lambda (xs, e) -> begin match cxt with
      | Top -> lambda fmt (xs, e)
      | _ -> fprintf fmt "@[(%a)@]" lambda (xs, e)
      end
    | Apply (fn, args) -> begin match cxt with
      | Top -> apply fmt (fn, args)
      | _ -> fprintf fmt "@[(%a)@]" apply (fn, args)
      end

  and lambda (fmt : formatter) ((xs,e) : id list * exp) : unit =
    fprintf fmt "@[lambda (%a) . %a" id_list xs (exp Top) e

  and apply (fmt : formatter) ((fn,args) : exp * exp list) : unit =
    fprintf fmt "@[%a(%a)@]" (exp Fun) fn exp_list args

  and exp_list (fmt : formatter) (exps : exp list) : unit = match exps with
    | [] -> ()
    | [e] -> exp Top fmt e
    | e :: es -> fprintf fmt "@[%a, %a@]" (exp Top) e exp_list es

  let rec value (fmt : formatter) (value : value) : unit = match value with
    | IntVal n -> fprintf fmt "%d" n
    | BoolVal b -> fprintf fmt "%b" b
    | ClosureVal (env_, xs, e) ->
      fprintf fmt "@[<%a> %a@]" env env_ lambda (xs, e)

  and env (fmt : formatter) (binds : (id * value) list) : unit =
    match binds with
      | [] -> ()
      | [(x,v)] -> fprintf fmt "@[%s = %a]" x value v
      | (x,v) :: binds' -> fprintf fmt "@[%s = %a; %a]" x value v env binds'

end

let format_exp = Format.exp Format.Top

let format_value = Format.value

let string_of_exp (e : exp) : string = make_string_of format_exp e

let print_exp (e : exp) : unit = print_string (string_of_exp e)

let string_of_value (v : value) : string = make_string_of format_value v

let print_value (v : value) : unit = print_string (string_of_value v)