{
  open Lexing
  open Arith_parser

  exception Error of string

  let int_of_string (str : string) : int =
    try
      int_of_string str
    with Failure _ ->
      raise (Error "integer literal out of range")
}

let blank = [ ' ' '\t' ]
let id = ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let decimal = '-'? ((['1'-'9']['0'-'9']*) | ['0'-'9'])

rule token = parse
  | "//" [^ '\n' '\r']+ { token lexbuf }
  | "\r\n" { new_line lexbuf; token lexbuf }
  | "\n" { new_line lexbuf; token lexbuf }
  | "/*" { block_comment lexbuf }
  | blank+ { token lexbuf }
  | eof { EOF }
  | decimal as n { INT (int_of_string n) } 
  | "+" { PLUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "*" { STAR }
  | "=" { EQUALS }
  | "in" { IN }
  | "let" { LET }
  | eof { EOF } 
  | id as x { ID x } (* by going last, we lex keywords instead of variables *)

and block_comment = parse
  | "*/" { token lexbuf }
  | "*" { block_comment lexbuf }
  | "\r\n" { new_line lexbuf; block_comment lexbuf }
  | "\n" { new_line lexbuf; block_comment lexbuf }
  | ([^ '*' '\n'])+  { block_comment lexbuf }
