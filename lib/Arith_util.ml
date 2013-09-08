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

let parse (str : string) : result = 
  parse_from_lexbuf (Lexing.from_string str)

module Format = struct

  open Format

  (* The concrete production that would have parsed the expression. *)
  type cxt = ATOM | MUL | ADD | EXP

  let print_paren (cxt : cxt) (e : exp) : bool = match e with
    | Let _ -> cxt < EXP
    | Id _ -> false
    | Int _ -> false
    | Mul _ -> cxt < MUL
    | Add _ -> cxt < ADD

  let rec exp (cxt : cxt) (fmt : formatter) (e : exp) : unit = 
    parens (print_paren cxt e) fmt (fun () ->
        match e with
        | Let (x, e1, e2) ->
          fprintf fmt "@[<v>let @[%s =@;<1 2>%a in@]@ %a@]" x 
            (exp EXP) e1 (exp EXP) e2
        | Mul (e1, e2) ->
          fprintf fmt "@[%a *@ %a@]" (exp MUL) e1 (exp ATOM) e2
        | Add (e1, e2) -> 
          fprintf fmt "@[%a +@ %a@]" (exp ADD) e1 (exp MUL) e2
        | Int n -> fprintf fmt "@[%d@]" n
        | Id x -> fprintf fmt "@[%s@]" x)


end

let format_exp = Format.exp Format.EXP

let string_of_exp (e : exp) : string = make_string_of format_exp e

let print_exp (e : exp) : unit = print_string (string_of_exp e)