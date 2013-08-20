type pos = Lexing.position

let string_of_pos pos = 
  let open Lexing in
  if String.length pos.pos_fname > 0 then
    Format.sprintf "%s, line %d, column %d" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
  else
    Format.sprintf "line %d, column %d" pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)

let make_string_of formatter x =
	let open Format in
  let buf = Buffer.create 100 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  formatter fmt x;
  fprintf fmt "@?";
  Buffer.contents buf
