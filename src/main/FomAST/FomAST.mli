open FomBasis
open FomSource
open FomPP

module LitString : sig
  include Map.OrderedType

  val of_utf8_json : string -> t
  val to_utf8_json : t -> string

  (* *)

  val of_utf8 : string -> t
  val to_utf8 : t -> string
end

module Kind : sig
  module Id : Id.S
  module Env : Map.S with type key = Id.t

  type 'k f = [`Star of Loc.t | `Arrow of Loc.t * 'k * 'k | `Var of Loc.t * Id.t]
  type t = [ | t f]

  val at : t -> Loc.t

  (* Comparison *)

  val compare : t cmp

  (* *)

  val min_arity : t -> int

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

    val equal : t bpr
    val compare : t cmp

    (* Kinding *)

    val kind_of : Loc.t -> t -> Kind.t

    (* Formatting *)

    val pp : t -> document
  end

  module Id : Id.S

  type ('t, 'k) f =
    [ `Mu of Loc.t * 't
    | `Const of Loc.t * Const.t
    | `Var of Loc.t * Id.t
    | `Lam of Loc.t * Id.t * 'k * 't
    | `App of Loc.t * 't * 't
    | `ForAll of Loc.t * 't
    | `Exists of Loc.t * 't
    | `Arrow of Loc.t * 't * 't
    | `Product of Loc.t * (Label.t * 't) list
    | `Sum of Loc.t * (Label.t * 't) list ]

  type t = [ | (t, Kind.t) f]

  val at : ('t, 'k) f -> Loc.t
  val set_at : Loc.t -> ('t, 'k) f uop

  (* Macros *)

  val product :
    Loc.t -> (Label.t * 't) list -> [> `Product of Loc.t * (Label.t * 't) list]

  val sum :
    Loc.t -> (Label.t * 't) list -> [> `Sum of Loc.t * (Label.t * 't) list]

  val zero : Loc.t -> [> `Sum of Loc.t * (Label.t * 't) list]

  (* Comparison *)

  val compare : t cmp

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
  val subst : Id.t -> t -> t uop
  val subst_par : t Env.t -> t uop
  val subst_rec : t Env.t -> t uop
  val norm : t -> t

  (* Freshening *)

  val freshen : t -> t

  (* Formatting *)

  val hanging : t -> (document * document) option
  val pp : ?pp_annot:(Kind.t -> document) -> t -> document
end

module Exp : sig
  module Const : sig
    type ('nat, 't) t =
      [ `LitBool of bool
      | `LitNat of 'nat
      | `LitString of LitString.t
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
      | `OpEq of 't
      | `OpEqNot of 't
      | `OpLogicalAnd
      | `OpLogicalNot
      | `OpLogicalOr ]

    (* Typing *)

    val type_of : Loc.t -> ('nat, Typ.t) t -> Typ.t

    (* Substitution *)

    val map_typ : ('t -> 'u) -> ('nat, 't) t -> ('nat, 'u) t

    val traverse_typ :
      ('t -> ('r, 'e, 'u) Rea.t) -> ('nat, 't) t -> ('r, 'e, ('nat, 'u) t) Rea.t

    val collect_typ : ('nat, 't) t -> 't list

    (* Comparison *)

    val compare' : 'nat cmp -> 't cmp -> ('nat, 't) t cmp

    (* Constants *)

    val lit_false : ('nat, 't) t
    val lit_true : ('nat, 't) t

    (* Formatting *)

    val pp' : ('nat -> document) -> ('t -> document) -> ('nat, 't) t -> document
    val pp : (Bigint.t, Typ.t) t -> document
  end

  module Id : Id.S
  module IdSet : Set.S with type elt = Id.t
  module Env : Map.S with type key = Id.t

  type ('e, 't, 'k) f =
    [ `Const of Loc.t * (Bigint.t, 't) Const.t
    | `Var of Loc.t * Id.t
    | `Lam of Loc.t * Id.t * 't * 'e
    | `App of Loc.t * 'e * 'e
    | `Gen of Loc.t * Typ.Id.t * 'k * 'e
    | `Inst of Loc.t * 'e * 't
    | `LetIn of Loc.t * Id.t * 'e * 'e
    | `Mu of Loc.t * 'e
    | `IfElse of Loc.t * 'e * 'e * 'e
    | `Product of Loc.t * (Label.t * 'e) list
    | `Select of Loc.t * 'e * Label.t
    | `Inject of Loc.t * Label.t * 'e
    | `Case of Loc.t * 'e
    | `Pack of Loc.t * 't * 'e * 't
    | `UnpackIn of Loc.t * Typ.Id.t * Id.t * 'e * 'e
    | `Target of Loc.t * 't * LitString.t ]

  type t = [ | (t, Typ.t, Kind.t) f]

  val at : ('e, 't, 'k) f -> Loc.t
end
