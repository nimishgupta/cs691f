open Typed_syntax
open Typed_util

let pp_test (s : string) : bool =
  match parse s with
    | ParseError s -> failwith ("error parsing string " ^ s)
    | Exp e ->
      let s' = string_of_exp e in
      if s = s' then true
      else (Format.printf "Parsed string:\n%s\nPretty-printed:\n%s\n%!" s s'; false)

(* These tests are copied verbatim from Arith_tests.ml *)

TEST "positive numerals" =
  parse "10" = Exp (Int 10)

TEST "overflow OCaml int" =
  let succ_maxint = Int64.to_string (Int64.succ (Int64.of_int max_int)) in
  match parse succ_maxint with
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

TEST "thunk parse-pretty" = pp_test "fun (x : num) -> 90"

TEST "curried function parse-pretty" =
  pp_test "fun (x : num) -> fun (y : num) -> x + y"

TEST "function application" =
  pp_test "e1 e2 e3"

TEST "function parenthesization" =
  pp_test "(fun (x : num) -> x + 1) 900"  
