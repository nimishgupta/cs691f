{
  open Lexing
  open Lexparse_util
  open HOF_parser
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
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | "." { DOT }
  | "+" { PLUS }
  | "-" { MINUS }  
  | "==" { EQUALSEQUALS }  
  | "*" { STAR }
  | "<" { LESSTHAN }
  | "lambda" { LAMBDA }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "true" { TRUE }
  | "false" { FALSE }
  | eof { EOF } 
  | id as x { ID x } (* by going last, we lex keywords instead of variables *)

and block_comment = parse
  | "*/" { token lexbuf }
  | "*" { block_comment lexbuf }
  | "\r\n" { new_line lexbuf; block_comment lexbuf }
  | "\n" { new_line lexbuf; block_comment lexbuf }
  | ([^ '*' '\n'])+  { block_comment lexbuf }
