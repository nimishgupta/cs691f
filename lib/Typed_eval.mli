open Typed_syntax

module type TYPECHECKER = sig
  exception Type_error of string
  val type_check : exp -> typ
end

val eval : exp -> exp

module Make : functor (TC : TYPECHECKER) -> sig end