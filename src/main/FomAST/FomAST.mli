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

  type 't f =
    [ `Mu of Loc.t * 't
    | `Const of Loc.t * Const.t
    | `Var of Loc.t * Id.t
    | `Lam of Loc.t * Id.t * Kind.t * 't
    | `App of Loc.t * 't * 't
    | `ForAll of Loc.t * 't
    | `Exists of Loc.t * 't
    | `Arrow of Loc.t * 't * 't
    | `Product of Loc.t * (Label.t * 't) list
    | `Sum of Loc.t * (Label.t * 't) list ]

  type t = [ | t f]

  val at : 't f -> Loc.t
  val set_at : Loc.t -> 't f -> 't f

  (* Macros *)

  val product :
    Loc.t -> (Label.t * 't) list -> [> `Product of Loc.t * (Label.t * 't) list]

  val sum :
    Loc.t -> (Label.t * 't) list -> [> `Sum of Loc.t * (Label.t * 't) list]

  val zero : Loc.t -> [> `Sum of Loc.t * (Label.t * 't) list]

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
    type ('nat, 't) t =
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
      | `OpEq of 't
      | `OpEqNot of 't
      | `OpLogicalAnd
      | `OpLogicalNot
      | `OpLogicalOr ]

    (* Typing *)

    val type_of : Loc.t -> ('nat, Typ.t) t -> Typ.t

    (* Substitution *)

    val map_typ : ('t -> 'u) -> ('nat, 't) t -> ('nat, 'u) t

    (* Constants *)

    val lit_false : ('nat, 't) t
    val lit_true : ('nat, 't) t

    (* Formatting *)

    val pp : (Bigint.t, Typ.t) t -> document
  end

  module Id : Id.S
  module Env : Map.S with type key = Id.t

  type ('e, 't) f =
    [ `Const of Loc.t * (Bigint.t, 't) Const.t
    | `Var of Loc.t * Id.t
    | `Lam of Loc.t * Id.t * 't * 'e
    | `App of Loc.t * 'e * 'e
    | `Gen of Loc.t * Typ.Id.t * Kind.t * 'e
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
    | `Target of Loc.t * 't * string ]

  type t = [ | (t, Typ.t) f]

  val at : ('e, 't) f -> Loc.t
end
