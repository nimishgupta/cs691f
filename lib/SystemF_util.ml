open SystemF_syntax
open PL_util

module Parsers = Lexparse_util.MakeParsers (struct
    exception ParseError = SystemF_parser.Error
    type token = SystemF_parser.token
    type exp = SystemF_sugar.cmd list
    let parser = SystemF_parser.commands
    let lexer = SystemF_lexer.token
  end)


open Parsers

type result = 
  | Command of SystemF_sugar.cmd list
  | ParseError of string 

let parse_from_lexbuf (lexbuf : Lexing.lexbuf) : result = 
  try Command (parse_from_lexbuf lexbuf)
  with Lexparse_util.Error str -> ParseError str

let parse_from_file (file_name : string) : result =
  try Command (parse_exp_from_file file_name)
  with Lexparse_util.Error str -> ParseError str

let parse (str : string) : result = 
  parse_from_lexbuf (Lexing.from_string str)
