open Typed_syntax
open Typed_util
exception Expected_error of exp
exception Fatal_error of exp

module type ENV = sig
  type t
 
  val empty : t
  val extend : id -> typ -> t -> t
  val lookup : id -> t -> typ
end

module Env : ENV = struct
  module M = Map.Make (String)
  type t = typ M.t
  let empty = M.empty
  let extend = M.add
  let lookup = M.find
end

let rec subst (x : id) (v : exp) (exp : exp) : exp = match exp with
  | Int _ -> exp
  | Bool _ -> exp
  | Arith (op, e1, e2) -> Arith (op, subst x v e1, subst x v e2)
  | Cmp (op, e1, e2) -> Cmp (op, subst x v e1, subst x v e2)
  | If (e1, e2, e3) -> If (subst x v e1, subst x v e2, subst x v e3)
  | Id y -> if x = y then v else exp
  | Let (y, e1, e2) -> Let (y, subst x v e1, if x = y then e2 else subst x v e2)
  | Fun (y, t, e) -> Fun (y, t, if x = y then e else subst x v e)
  | Fix (y, t, e) -> Fix (y, t, if x = y then e else subst x v e)
  | App (e1, e2) -> App (subst x v e1, subst x v e2)
  | Empty t -> exp
  | Cons (e1, e2) -> Cons (subst x v e1, subst x v e2)
  | Head e -> Head (subst x v e)
  | Tail e -> Tail (subst x v e)
  | IsEmpty e -> IsEmpty (subst x v e)
  | Pair (e1, e2) -> Pair (subst x v e1, subst x v e2)
  | ProjL e -> ProjL (subst x v e)
  | ProjR e -> ProjR (subst x v e)

let arithOp (op : arithOp) : int -> int -> int = match op with
  | Plus -> (+)
  | Minus -> (-)
  | Times -> ( * )

let intCmp (op : intCmp) : int -> int -> bool = match op with
  | LT -> (<)
  | GT -> (>)
  | EQ -> (=)

let to_int (v : exp) : int = match v with
  | Int n -> n
  | _ -> failwith "to_int expected number"

let to_bool (v : exp) : bool = match v with
  | Bool b -> b
  | _ -> failwith "to_bool expected boolean"

let rec eval (exp : exp) : exp = match exp with
  | Int _ -> exp
  | Bool _ -> exp
  | Arith (op, e1, e2) -> (match (eval e1, eval e2) with
    | Int n1, Int n2 -> Int (arithOp op n1 n2)
    | v1, v2 -> raise (Fatal_error (Arith (op, v1, v2))))
  | Cmp (op, e1, e2) -> (match (eval e1, eval e2) with
    | Int n1, Int n2 -> Bool (intCmp op n1 n2)
    | v1, v2 -> raise (Fatal_error (Cmp (op, v1, v2))))
  | If (e1, e2, e3) ->
    if to_bool (eval e1) then eval e2 else eval e3
  | Id x -> raise (Fatal_error (Id x))
  | Let (x, e1, e2) -> eval (subst x (eval e1) e2)
  | Fun (x, t, e) -> exp
  | Fix (x, t, e) -> eval (subst x (Fix (x, t, e)) (eval e))
  | App (e1, e2) -> (match eval e1 with
    | Fun (x, _, e1') -> eval (subst x (eval e2) e1')
    | v1 -> raise (Fatal_error (App (v1, e2))))
  | Empty _ -> exp
  | Cons (e1, e2) -> Cons (eval e1, eval e2)
  | Head e -> (match eval e with
    | Cons (v1, v2) -> v1
    | Empty t -> raise (Expected_error ((Head (Empty t))))
    | v -> raise (Fatal_error (Head v)))
  | Tail e -> (match eval e with
    | Cons (v1, v2) -> v2
    | Empty t -> raise (Expected_error ((Tail (Empty t))))
    | v -> raise (Fatal_error (Tail v)))
  | IsEmpty e -> (match eval e with
    | Cons _ -> Bool false
    | Empty _ -> Bool true
    | v -> raise (Fatal_error (IsEmpty v)))
  (* Evaluation of pairs *)
  | Pair (e1, e2) -> Pair (eval e1, eval e2)
  | ProjL e -> (match eval e with
    | Pair (v1, v2) -> v1
    | v -> raise (Fatal_error (ProjL v)))
  | ProjR e -> (match eval e with
    | Pair (v1, v2) -> v2
    | v -> raise (Fatal_error (ProjR v)))
  (* END *)

module type TYPECHECKER = sig
  exception Type_error of string
  val type_check : exp -> typ
end

module type EVALUATOR = sig
  val eval : exp -> exp
end

module Make (TC : TYPECHECKER) = struct

  let rec repl () : unit = 
    print_string "> ";
    match Typed_util.parse (read_line ()) with
    | Typed_util.Exp exp ->
      (try 
        let typ = TC.type_check exp in
        print_string "Type check OK.";
        print_typ typ;
        print_newline ();
        print_string  (string_of_exp (eval exp));
        print_newline ()
      with
        | TC.Type_error msg ->
            Format.printf "Type error:\n%s\n" msg
        | Expected_error exp ->
            Format.printf "Expected runtime error:\n%s\n" (string_of_exp exp)
        | Fatal_error exp ->
            Format.printf "Unexpected runtime error:\n%s\n" (string_of_exp exp));
      repl ()      
    | Typed_util.ParseError msg ->
      Format.printf "Parser/lexer error:\n%s\n" msg;
      repl ()

  let _ =  
    match Array.to_list Sys.argv with
    | [ exe; "repl" ] -> print_string "Press Ctrl + C to quit.\n"; repl ()
    | _ -> ()

end