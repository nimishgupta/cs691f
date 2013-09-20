(** Evaluator for the typed language *)
open Typed_syntax

val eval : exp -> exp

(** The signature expected of typechecker *)
module type TYPECHECKER = sig
  exception Type_error of string
  val type_check : exp -> typ
end

(** When applied, this functor creates a REPL that runs the provided
    type checker and the evaluator. You can start the REPL by running:

    [cs691f run Typechecker repl]
  *)
module Make : functor (TC : TYPECHECKER) -> sig end