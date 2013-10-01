module TC = SystemF_tc
module Term = ANSITerminal

open SystemF_eval
open SystemF_util

let rec repl decls subst : unit = 
  print_string "> ";
  let text = read_line () in
  match SystemF_util.parse text with 
  | SystemF_util.Command cmds -> eval_cmd decls subst cmds
  | SystemF_util.ParseError msg ->
    Term.printf [Term.yellow] "Parser/lexer error:\n%s\n%!" msg;
    repl decls subst

and eval_cmd (decls : SystemF_sugar.decls) (subst : SystemF_syntax.exp -> SystemF_syntax.exp) cmds : unit = match cmds with
  | [] -> repl decls subst
  | SystemF_sugar.NewType (x, abbrv) :: cmds' -> 
    eval_cmd ((x, abbrv) :: decls) subst cmds'
  | SystemF_sugar.NamedExp (x, e) :: cmds' ->
    let e = SystemF_sugar.desugar decls subst e in
    eval_cmd decls (fun exp -> subst (SystemF_subst.subst x e exp)) cmds'
  | (SystemF_sugar.Exp e) :: cmds' ->
    (try 
      let exp = SystemF_sugar.desugar decls subst e in
      let typ = TC.typecheck exp in
      Term.printf [Term.green] "OK. Expression has type:\n%s\nResult is:\n%s\n%!"
        (string_of_typ typ) (string_of_exp (eval exp))
    with
      | TC.Type_error (p, msg) ->
          Term.printf [Term.red] "Type error at %s:\n%s\n%!" 
          (Pos.to_string p) msg;
      | Expected_error exp ->
          Term.printf [Term.blue] "Expected runtime error:\n%s\n%!" (string_of_exp exp)
      | Fatal_error exp ->
          Term.printf [Term.black; Term.on_red] "Unexpected runtime error:\n%s\n%!" (string_of_exp exp));
    eval_cmd decls subst cmds'

let _ =  
  match Array.to_list Sys.argv with
  | [ exe ] -> print_string "Press Ctrl + C to quit.\n"; repl [] (fun x -> x)
  | [ exe; prelude_file ] -> 
     (match (parse_from_file prelude_file) with
     | SystemF_util.Command cmds -> eval_cmd [] (fun x -> x) cmds
     | SystemF_util.ParseError msg ->
         Term.printf [Term.yellow] "Parser/lexer error:\n%s\n%!" msg)
  | _ ->
    print_string "Too many arguments on command line.\n"
