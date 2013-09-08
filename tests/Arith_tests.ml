open Arith_syntax
open Arith_util

let pp_test (s : string) : bool =
  match parse s with
    | ParseError msg -> failwith ("error parsing string: " ^ msg)
    | Exp e ->
      let s' = string_of_exp e in
      if s = s' then true
      else (Format.printf "Parsed string:\n%s\nPretty-printed:\n%s\n%!" s s'; false)

TEST "positive numerals" =
	parse "10" = Exp (Int 10)

TEST "overflow OCaml int" =
  let succ_maxint = Int64.to_string (Int64.succ (Int64.of_int max_int)) in
  match parse succ_maxint with
    | ParseError _ -> true
    | _ -> false

TEST "negative numerals" = 
  parse "-1" = Exp (Int (-1))

TEST "+,* precedence" =
  parse "1 + 2 * 3 + 4" =
    Exp (Add (Add (Int 1, Mul (Int 2, Int 3)), Int 4))

TEST "let nested in body" =
  parse "let x = 10 in let y = 11 in x" =
    Exp (Let ("x", Int 10,
           Let ("y", Int 11,
             Id "x")))

TEST "let nested in binding" =
  parse "let x = let y = 11 in y in x" =
    Exp (Let ("x", Let ("y", Int 11, Id "y"),
             Id "x"))

TEST "let with nested *" =
  parse "let x = 9 in 11 * 10" =
    Exp (Let ("x", Int 9, Mul (Int 11, Int 10)))

TEST "simple comment" =
  parse "/* comment */10" = Exp (Int 10)

TEST "commented /*" =
  parse "/* /* */10"  = Exp (Int 10)

TEST "multi-line comment" =
  parse "/*
                          * multi-line comment
                          */ 10"
    = Exp (Int 10)

TEST "let reserved (binder)" =
  match parse "let let = 10 in 10" with
    | ParseError _ -> true
    | _ -> false

TEST "let reserved (identifier)" =
  match parse "(let + 10)" with
    | ParseError _ -> true
    | _ -> false

TEST "prety-print parentheses around +" =
  pp_test "1 + (2 + 3)"

TEST "do not parenthesize" =
  pp_test "1 + 2 * 3 + 4"

TEST "precendence" =
  pp_test "(1 + 2) * (3 + 4)"

TEST "line break" = pp_test "\
1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 +
1"

TEST "margin for *" = pp_test "\
(1 + 2 + 3 + 4 + 5) * (1 + 2 + 3 + 4 + 5) * (1 + 2 + 3 + 4 + 5) *
(1 + 2 + 3 + 4 + 5)"

TEST "margin for +" = pp_test "\
1 * 2 * 3 * 4 * 5 + 1 * 2 * 3 * 4 * 5 + 1 * 2 * 3 * 4 * 5 + 1 * 2 * 3 * 4 * 5 +
1 * 2 * 3 * 4 * 5"

TEST "let nesting" = pp_test "\
let x = 10 in
let x = 10 in
let x = 10 in
10"

TEST "parenthesize let within + and *" = pp_test "\
10 + (let x = 15 in
      x)"