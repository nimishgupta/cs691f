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
     or not we need parentheses. We use LET when there is no context or the
     context is a parenthesis, but that may be a bad idea. *)
  type cxt = LET | MUL | ADD

  let rec exp (cxt : cxt) (fmt : formatter) (e : exp) : unit = match e with
    | Let (x, e1, e2) ->
      fprintf fmt "@[let %s = %a in %a@]" x (exp LET) e1 (exp LET) e2
    | Mul (e1, e2) -> begin match cxt with
      | ADD -> fprintf fmt "@[(@[%a * %a@])@]" (exp MUL) e1 (exp MUL) e2
      | LET
      | MUL -> fprintf fmt "@[%a * %a@]" (exp MUL) e1 (exp MUL) e2
    end
    | Add (e1, e2) -> begin match cxt with
      | LET
      | ADD -> fprintf fmt "@[%a + %a@]" (exp ADD) e1 (exp ADD) e2
      | MUL -> fprintf fmt "@[(@[%a + %a@])@]" (exp ADD) e1 (exp ADD) e2
    end
    | Int n -> fprintf fmt "@[%d@]" n
    | Id x -> fprintf fmt "@[%s@]" x

end

let format_exp = Format.exp Format.LET

let string_of_exp (e : exp) : string = make_string_of format_exp e

let print_exp (e : exp) : unit = print_string (string_of_exp e)