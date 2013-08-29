open HOF_syntax
open HOF_util

let pp_test (s : string) : bool =
  match parse_exp_from_string s with
    | ParseError s -> failwith ("error parsing string " ^ s)
    | Exp e ->
      let s' = string_of_exp e in
      if s = s' then true
      else (Format.printf "Parsed string:\n%s\nPretty-printed:\n%s\n%!" s s'; false)

(* These tests are copied verbatim from Arith_tests.ml *)

TEST "positive numerals" =
  parse_exp_from_string "10" = Exp (Int 10)

TEST "overflow OCaml int" =
  let succ_maxint = Int64.to_string (Int64.succ (Int64.of_int max_int)) in
  match parse_exp_from_string succ_maxint with
    | ParseError _ -> true
    | _ -> false

TEST "negative numerals" = 
  parse_exp_from_string "-1" = Exp (Int (-1))

TEST "+,* precedence" =
  parse_exp_from_string "1 + 2 * 3 + 4" =
    Exp (Add (Add (Int 1, Mul (Int 2, Int 3)), Int 4))

TEST "let nested in body" =
  parse_exp_from_string "let x = 10 in let y = 11 in x" =
    Exp (Let ("x", Int 10,
           Let ("y", Int 11,
             Id "x")))

TEST "let nested in binding" =
  parse_exp_from_string "let x = let y = 11 in y in x" =
    Exp (Let ("x", Let ("y", Int 11, Id "y"),
             Id "x"))

TEST "let with nested *" =
  parse_exp_from_string "let x = 9 in 11 * 10" =
    Exp (Let ("x", Int 9, Mul (Int 11, Int 10)))

TEST "simple comment" =
  parse_exp_from_string "/* comment */10" = Exp (Int 10)

TEST "commented /*" =
  parse_exp_from_string "/* /* */10"  = Exp (Int 10)

TEST "multi-line comment" =
  parse_exp_from_string "/*
                          * multi-line comment
                          */ 10"
    = Exp (Int 10)

TEST "let reserved (binder)" =
  match parse_exp_from_string "let let = 10 in 10" with
    | ParseError _ -> true
    | _ -> false

TEST "let reserved (identifier)" =
  match parse_exp_from_string "(let + 10)" with
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

(* End of tests from Arith_tests.ml *)

TEST "thunk parse-pretty" = pp_test "lambda() . 90"

TEST "unary function parsing" = 
  parse_exp_from_string "lambda(x) . 90" = Exp (Lambda (["x"], Int 90))

TEST "binary function parsing" = 
  parse_exp_from_string "lambda(x, y) . 90" = Exp (Lambda (["x"; "y"], Int 90))

TEST "curried function parse-pretty" =
  pp_test "lambda(x) . lambda(y) . x + y"

TEST "duplicate identifier" =
  match parse_exp_from_string "lambda(x,x) . x" with
  | Exp _ -> false
  | ParseError _ ->  true

TEST "record parse-pretty" = pp_test "{ x: 10, y: 20 }"

TEST "empty record parse-pretty" = pp_test "{ }"

TEST "record update parse-pretty" = pp_test "x[y -> 10]"

TEST "record update chain" = pp_test "x[y -> 10][y -> 20]"

TEST "record get chain" = pp_test "x.y.z.a.b.c"

TEST "long record" = pp_test "\
{
  foooooooooooo: 0,
  foooooooooooo1: 0,
  foooooooooooo2: 0,
  foooooooooooo3: 0,
  foooooooooooo4: 0
}"