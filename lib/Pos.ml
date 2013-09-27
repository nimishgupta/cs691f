module Term = ANSITerminal

type t  = {
  start : Lexing.position;
  ending : Lexing.position
}

let highlight = [ ANSITerminal.on_red; ANSITerminal.yellow ]

let compare = Pervasives.compare

let mk start ending = { start; ending }

let to_string (p : t) : string =
  let open Lexing in
  let buf = Buffer.create 50 in
  let pr = Buffer.add_string buf in
  (if p.start.pos_fname = "" then
    pr "line "
   else
     (pr p.start.pos_fname;
      pr ", line "));
  pr (string_of_int p.start.pos_lnum);
  pr " column ";
  pr (string_of_int (p.start.pos_cnum - p.start.pos_bol));
  (if p.start.pos_lnum = p.ending.pos_lnum then
     (pr " -- ";
      pr (string_of_int (p.ending.pos_cnum - p.ending.pos_bol)))
   else
     (pr " line ";
      pr (string_of_int p.ending.pos_lnum);
      pr " column ";
      pr (string_of_int (p.ending.pos_cnum - p.ending.pos_bol))));
  Buffer.contents buf

let print (p : t) (str : string) = 
  let start_lnum = p.start.Lexing.pos_lnum in
  let end_lnum = p.ending.Lexing.pos_lnum in
  let print_suffix (lines : string list) : unit =
    List.iter (fun line -> print_string line; print_newline ()) lines in
  let print_last_highlighted_line line lines : unit = 
    print_string ">>> ";
    print_string line;
    print_suffix lines in
  let rec print_highlighted_lines (lnum : int) (lines : string list) : unit =
    match lines with
    | [] -> ()
    | line :: lines ->
      if lnum = end_lnum then
        print_last_highlighted_line line lines
      else 
        (print_string ">>> ";
         print_string line;
         print_highlighted_lines (1 + lnum) lines) in
  let print_first_line_highlight lnum line lines : unit =
    let start = p.start.Lexing.pos_cnum - p.start.Lexing.pos_bol in
    let ending = if p.start.Lexing.pos_lnum <> p.start.Lexing.pos_lnum then
                String.length line
              else
                p.ending.Lexing.pos_cnum - p.ending.Lexing.pos_bol in
    let prefix = String.sub line 0 start in
    let highlighted = String.sub line start (ending - start) in
    let suffix = String.sub line ending (String.length line - ending) in
    Term.print_string [] prefix;
    Term.print_string highlight highlighted;
    Term.print_string [] suffix;
    print_newline ();
    print_highlighted_lines (1 + lnum) lines in
  let rec print_prefix (lnum : int) (lines : string list) : unit =
    match lines with
    | [] -> ()
    | line :: lines ->
      if lnum < start_lnum then 
        (print_string line;
         print_prefix (1 + lnum) lines)
      else
        print_first_line_highlight lnum line lines
  in print_prefix 1 (Str.split (Str.regexp "$") str)

