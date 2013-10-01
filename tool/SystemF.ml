module TC = SystemF_tc
module Term = ANSITerminal

open SystemF_eval
open SystemF_util

let rec repl decls : unit = 
  print_string "> ";
  let text = read_line () in
  match SystemF_util.parse text with 
  | SystemF_util.Command cmds -> eval_cmd decls cmds
  | SystemF_util.ParseError msg ->
    Term.printf [Term.yellow] "Parser/lexer error:\n%s\n%!" msg;
    repl decls

and eval_cmd decls cmds = match cmds with
  | [] -> repl decls
  | SystemF_sugar.NewAbbrv (x, abbrv) :: cmds' -> 
    eval_cmd ((x, abbrv) :: decls) cmds'
  | (SystemF_sugar.Exp e) :: cmds' ->
    (try 
      let exp = SystemF_sugar.desugar decls e in
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
    eval_cmd decls cmds'

let rec filter_map f xs = match xs with
  | [] -> []
  | x :: xs' -> match f x with
    | Some y -> y :: (filter_map f xs')
    | None -> filter_map f xs'

let select_newabbrv abbrv = match abbrv with
  | SystemF_sugar.NewAbbrv (x, e) -> Some (x,e)
  | SystemF_sugar.Exp _ -> None

let _ =  
  match Array.to_list Sys.argv with
  | [ exe ] -> print_string "Press Ctrl + C to quit.\n"; repl []
  | [ exe; prelude_file ] -> 
     (match (parse_from_file prelude_file) with
     | SystemF_util.Command cmds -> eval_cmd [] cmds
     | SystemF_util.ParseError msg ->
         Term.printf [Term.yellow] "Parser/lexer error:\n%s\n%!" msg)
  | _ ->
    print_string "Too many arguments on command line.\n"
