open Arith_syntax
open Arith_util

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
    Exp (Mul (Add (Int 1, Int 2), Add (Int 3, Int 4)))

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