open StdlibPlus
open FomAST

module Lam : sig
  module Var : sig
    val to_js : Exp.Var.t -> Cats.t
  end

  type t =
    [ `App of t * t
    | `Case of t
    | `Const of (int32, Typ.Core.t) Exp.Const.t
    | `IfElse of t * t * t
    | `Inject of Label.t * t
    | `Lam of Exp.Var.t * t
    | `Mu of t
    | `Product of (Label.t * t) list
    | `Select of t * t
    | `Var of Exp.Var.t ]
end

val erase : Exp.Core.t -> Lam.t
(** Erase (most) types from given Fωμ expression.  Note that this does not type
    check the expression. *)

val simplify : Lam.t -> ('r, 'e, Lam.t) rea
(** Simplify erased expression. *)

val to_js :
  top:
    [< `Body
    | `Return
    | `Seq
    | `Tail of Exp.Var.t * Exp.Var.t list * Exp.Var.t list * [`Exit | `Case]
    | `Top ] ->
  Lam.t ->
  ('r, 'e, Cats.t) rea
(** Transpile erased expression to JavaScript. *)
