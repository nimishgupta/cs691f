open PL_util

exception Error of string

let int_of_string (str : string) : int =
  try
    int_of_string str
  with Failure _ ->
    raise (Error "integer literal out of range")

module type PARSER = sig
  exception ParseError
  type token
  type exp

  val parser : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> exp
  val lexer : Lexing.lexbuf -> token
end

module MakeParsers (Parser : PARSER) = struct

  open Parser
  
  let parse_from_lexbuf (lexbuf : Lexing.lexbuf) : 'exp =
    let open Lexing in
    let open Format in  
    try
      parser lexer lexbuf
    with
      | Failure "lexing: empty token" ->
         raise (Error (sprintf "lexical error at %s" 
                         (string_of_pos lexbuf.lex_curr_p)))
      | Error str ->
         raise (Error (sprintf "%s (lexical error at %s)" 
                         str
                         (string_of_pos lexbuf.lex_curr_p)))
       | ParseError ->
         raise (Error (sprintf "parse error at %s; unexpected token %s"
                        (string_of_pos lexbuf.lex_curr_p)
                        (lexeme lexbuf)))

let parse_exp_from_file (file_name : string) : exp =
  let open Lexing in
  let lexbuf = from_channel (open_in file_name) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file_name };
  parse_from_lexbuf lexbuf

end