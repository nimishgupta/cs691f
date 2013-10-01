{
  open Lexing
  open Lexparse_util
  open SystemF_parser
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
  | "." { DOT }
  | ":" { COLON }
  | "<" { LANGLE }
  | ">" { RANGLE }
  | "->" { RARROW }
  | "=" { EQUALS }
  | ";;" { SEMISEMI }
  | "forall" { FORALL }
  | "fun" { FUN }
  | "typfun" { TYPFUN }
  | "int" { INT_TYPE }
  | "type" { TYPE }
  | "let" { LET }
  | "[[" { LLBRACKET }
  | "]]" { RRBRACKET }
  | eof { EOF } 
  | id as x { ID (Identifier.from_string x) }

and block_comment = parse
  | "*/" { token lexbuf }
  | "*" { block_comment lexbuf }
  | "\r\n" { new_line lexbuf; block_comment lexbuf }
  | "\n" { new_line lexbuf; block_comment lexbuf }
  | ([^ '*' '\n'])+  { block_comment lexbuf }
