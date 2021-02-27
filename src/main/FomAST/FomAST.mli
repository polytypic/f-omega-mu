open FomSource
open FomPP

module Kind : sig
  type t = [`Star of Loc.t | `Arrow of Loc.t * t * t]

  val at : t -> Loc.t

  (* Comparison *)

  val compare : t -> t -> int

  (* Formatting *)

  val pp : t -> document
end

module Label : Id.S

module Typ : sig
  module Const : sig
    type t =
      [ `Arrow
      | `Bool
      | `Int
      | `Product of Label.t list
      | `Sum of Label.t list
      | `String ]

    (* Comparison *)

    val equal : t -> t -> bool
    val compare : t -> t -> int

    (* Kinding *)

    val kind_of : Loc.t -> t -> Kind.t

    (* Formatting *)

    val pp : t -> document
  end

  module Id : Id.S

  type t =
    [ `Mu of Loc.t * t
    | `Const of Loc.t * Const.t
    | `Var of Loc.t * Id.t
    | `Lam of Loc.t * Id.t * Kind.t * t
    | `App of Loc.t * t * t
    | `ForAll of Loc.t * t
    | `Exists of Loc.t * t ]

  val at : t -> Loc.t

  (* Macros *)

  val arrow : Loc.t -> t -> t -> t
  val var_of_label : Label.t -> t
  val product : Loc.t -> (Label.t * t) list -> t
  val sum : Loc.t -> (Label.t * t) list -> t

  (* Comparison *)

  val compare : t -> t -> int

  (*  *)

  val linearize : t -> t * t list
  val arity_and_result : t -> int * t
  val is_int : t -> bool
  val is_bool : t -> bool

  (* Substitution *)

  val is_free : Id.t -> t -> bool
  val subst : ?replaced:(Id.t -> unit) -> Id.t -> t -> t -> t

  (* Formatting *)

  val pp : t -> document
end

module Exp : sig
  module Const : sig
    type t =
      [ `LitBool of bool
      | `LitNat of Bigint.t
      | `LitString of string
      | `OpArithAdd
      | `OpArithDiv
      | `OpArithMinus
      | `OpArithMul
      | `OpArithPlus
      | `OpArithRem
      | `OpArithSub
      | `OpCmpGt
      | `OpCmpGtEq
      | `OpCmpLt
      | `OpCmpLtEq
      | `OpEq of Typ.t
      | `OpEqNot of Typ.t
      | `OpLogicalAnd
      | `OpLogicalNot
      | `OpLogicalOr ]

    (* Typing *)

    val type_of : Loc.t -> t -> Typ.t

    (* Substitution *)

    val subst : Typ.Id.t -> Typ.t -> t -> t

    (* Constants *)

    val lit_false : t
    val lit_true : t

    (* Formatting *)

    val pp : t -> document
  end

  module Id : Id.S

  type 't f =
    [ `Const of Loc.t * Const.t
    | `Var of Loc.t * Id.t
    | `Lam of Loc.t * Id.t * Typ.t * 't
    | `App of Loc.t * 't * 't
    | `Gen of Loc.t * Typ.Id.t * Kind.t * 't
    | `Inst of Loc.t * 't * Typ.t
    | `LetIn of Loc.t * Id.t * 't * 't
    | `Mu of Loc.t * 't
    | `IfElse of Loc.t * 't * 't * 't
    | `Product of Loc.t * (Label.t * 't) list
    | `Select of Loc.t * 't * Label.t
    | `Inject of Loc.t * Label.t * 't * Typ.t
    | `Case of Loc.t * 't * 't
    | `Pack of Loc.t * Typ.t * 't * Typ.t
    | `UnpackIn of Loc.t * Typ.Id.t * Id.t * 't * 't
    | `Target of Loc.t * Typ.t * string ]

  type t = [ | t f]

  val at : 't f -> Loc.t

  (* Substitution *)

  val is_free : Id.t -> t -> bool
  val subst : Id.t -> t -> t -> t
end
