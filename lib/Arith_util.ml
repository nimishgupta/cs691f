open Arith_syntax
open PL_util

module Parsers = Lexparse_util.MakeParsers (struct
  exception ParseError = Arith_parser.Error
  type token = Arith_parser.token
  type exp = Arith_syntax.exp
  let parser = Arith_parser.program
  let lexer = Arith_lexer.token
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
     or not we need parentheses.  *)
  type cxt = Top | LeftMul | RightMul | LeftAdd | RightAdd

  let print_paren (cxt : cxt) (e : exp) : bool = match e with
    | Let _ -> cxt > Top
    | Mul _ -> cxt > LeftMul
    | Add _ -> cxt > LeftAdd
    | _ -> false

  let rec exp (cxt : cxt) (fmt : formatter) (e : exp) : unit = 
    parens (print_paren cxt e) fmt (fun () ->
    match e with
    | Let (x, e1, e2) ->
      fprintf fmt "@[<v>let @[%s =@;<1 2>%a in@]@ %a@]" x 
        (exp Top) e1 (exp Top) e2
    | Mul (e1, e2) ->
      fprintf fmt "@[%a *@ %a@]" (exp LeftMul) e1 (exp RightMul) e2
    | Add (e1, e2) -> 
        fprintf fmt "@[%a +@ %a@]" (exp LeftAdd) e1 (exp RightAdd) e2
    | Int n -> fprintf fmt "@[%d@]" n
    | Id x -> fprintf fmt "@[%s@]" x)


end

let format_exp = Format.exp Format.Top

let string_of_exp (e : exp) : string = make_string_of format_exp e

let print_exp (e : exp) : unit = print_string (string_of_exp e)