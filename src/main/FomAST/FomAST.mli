open FomSource
open FomPP

module Kind : sig
  type t = [`Star of Loc.t | `Arrow of Loc.t * t * t]

  val at : t -> Loc.t

  (* Comparison *)

  val compare : t -> t -> int

  (* *)

  val arity : t -> int

  (* Formatting *)

  val pp : t -> document
  val pp_annot : t -> document
end

module Label : Id.S

module Tuple : sig
  val is_tuple : (Label.t * 'a) list -> bool
end

module Typ : sig
  module Const : sig
    type t = [`Bool | `Int | `String]

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
    | `Exists of Loc.t * t
    | `Arrow of Loc.t * t * t
    | `Product of Loc.t * (Label.t * t) list
    | `Sum of Loc.t * (Label.t * t) list ]

  val at : t -> Loc.t
  val set_at : Loc.t -> t -> t

  (* Macros *)

  val product : Loc.t -> (Label.t * t) list -> t
  val sum : Loc.t -> (Label.t * t) list -> t
  val zero : Loc.t -> t

  (* Comparison *)

  val compare : t -> t -> int

  (* Type predicates *)

  val is_int : t -> bool

  (* Type applications *)

  val app : Loc.t -> t -> t list -> t
  val unapp : t -> t * t list
  val arity_and_result : t -> int * t

  (* Substitution *)

  module IdSet : Set.S with type elt = Id.t
  module Env : Map.S with type key = Id.t

  val free : t -> IdSet.t
  val is_free : Id.t -> t -> bool
  val subst : ?replaced:(Id.t -> t -> unit) -> Id.t -> t -> t -> t
  val subst_par : ?replaced:(Id.t -> t -> unit) -> t Env.t -> t -> t
  val subst_rec : ?replaced:(Id.t -> t -> unit) -> t Env.t -> t -> t
  val norm : t -> t

  (* Formatting *)

  val hanging : t -> (document * document) option
  val pp : t -> document
end

module Exp : sig
  module Const : sig
    type 'nat t =
      [ `LitBool of bool
      | `LitNat of 'nat
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

    val type_of : Loc.t -> 'nat t -> Typ.t

    (* Substitution *)

    val subst_par :
      ?replaced:(Typ.Id.t -> Typ.t -> unit) ->
      Typ.t Typ.Env.t ->
      'nat t ->
      'nat t

    (* Constants *)

    val lit_false : 'nat t
    val lit_true : 'nat t

    (* Formatting *)

    val pp : Bigint.t t -> document
  end

  module Id : Id.S
  module Env : Map.S with type key = Id.t

  type 't f =
    [ `Const of Loc.t * Bigint.t Const.t
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
end
