open FomBasis
open FomAST

module Erased : sig
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

val erase : Exp.Core.t -> Erased.t
(** Erase (most) types from given Fωμ expression.  Note that this does not type
    check the expression. *)

val simplify : Erased.t -> ('r, 'e, Erased.t) rea
(** Simplify erased expression. *)

val to_js :
  ?top:[`Const of Exp.Var.t | `Return | `Top] ->
  Erased.t ->
  ('r, 'e, Cats.t) rea
(** Transpile erased expression to JavaScript. *)
