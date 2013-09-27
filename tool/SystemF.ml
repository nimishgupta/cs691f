module TC = SystemF_tc
module Term = ANSITerminal

open SystemF_eval
open SystemF_util

let rec subst_prelude prelude exp =
  match prelude with
  | [] -> exp
  | (x, e) :: rest -> subst_prelude rest (subst x e exp)

let rec repl prelude : unit = 
  print_string "> ";
  match SystemF_util.parse (read_line ()) with
  | SystemF_util.Exp exp ->
    (try 
      let exp = subst_prelude prelude exp in
      let typ = TC.typecheck exp in
      Term.printf [Term.green] "OK. Expression has type:\n%s\nResult is:\n%s\n%!"
        (string_of_typ typ) (string_of_exp (eval exp))
    with
      | TC.Type_error (p, msg) ->
          Term.printf [Term.red] "Type error at %s:\n%s\n%!" 
          (PL_util.string_of_pos p)  msg
      | Expected_error exp ->
          Term.printf [Term.blue] "Expected runtime error:\n%s\n%!" (string_of_exp exp)
      | Fatal_error exp ->
          Term.printf [Term.black; Term.on_red] "Unexpected runtime error:\n%s\n%!" (string_of_exp exp));
    repl prelude
  | SystemF_util.ParseError msg ->
    Term.printf [Term.yellow] "Parser/lexer error:\n%s\n%!" msg;
    repl prelude

let _ =  
  match Array.to_list Sys.argv with
  | [ exe ] -> print_string "Press Ctrl + C to quit.\n"; repl []
  | [ exe; prelude_file ] -> 
     let prelude = parse_prelude_from_file prelude_file in
     print_string "Press Ctrl + C to quit.\n"; repl prelude
  | _ ->
    print_string "Too many arguments on command line.\n"
