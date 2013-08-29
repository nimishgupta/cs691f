{
  open Lexing
  open Lexparse_util
  open HOF_parser
}

let blank = [ ' ' '\t' ]
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
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
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "," { COMMA }
  | "." { DOT }
  | ":" { COLON }
  | "+" { PLUS }
  | "-" { MINUS }  
  | "*" { STAR }
  | "=" { EQUALS }
  | "->" { RARROW }
  | "lambda" { LAMBDA }
  | "if0" { IF0 }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | eof { EOF } 
  | id as x { ID x }

and block_comment = parse
  | "*/" { token lexbuf }
  | "*" { block_comment lexbuf }
  | "\r\n" { new_line lexbuf; block_comment lexbuf }
  | "\n" { new_line lexbuf; block_comment lexbuf }
  | ([^ '*' '\n'])+  { block_comment lexbuf }
