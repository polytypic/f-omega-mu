open FomBasis
open FomSource
open FomPP

module Kind : sig
  module Unk : Id.S
  module UnkMap : Map.S with type key = Unk.t

  type 'k f =
    [`Star of Loc.t | `Arrow of Loc.t * 'k * 'k | `Unk of Loc.t * Unk.t]

  type t = t f

  val at : t -> Loc.t

  (* *)

  val fresh : Loc.t -> t

  (* Comparison *)

  val compare : t cmp

  (* *)

  val min_arity : t -> int

  (* *)

  val keep_phys_eq' : t -> t -> t
  val keep_phys_eq : (t -> t) -> t -> t

  (* Formatting *)

  module Numbering : sig
    type t

    val create : unit -> t
  end

  val pp : ?numbering:Numbering.t -> t -> document
  val pp_annot : ?numbering:Numbering.t -> t -> document
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

  module Var : Id.S
  module VarSet : Set.S with type elt = Var.t
  module VarMap : Map.S with type key = Var.t

  type ('t, 'k) f =
    [ `Mu of Loc.t * 't
    | `Const of Loc.t * Const.t
    | `Var of Loc.t * Var.t
    | `Lam of Loc.t * Var.t * 'k * 't
    | `App of Loc.t * 't * 't
    | `ForAll of Loc.t * 't
    | `Exists of Loc.t * 't
    | `Arrow of Loc.t * 't * 't
    | `Product of Loc.t * (Label.t * 't) list
    | `Sum of Loc.t * (Label.t * 't) list ]

  type t = (t, Kind.t) f

  val at : ('t, 'k) f -> Loc.t
  val set_at : Loc.t -> ('t, 'k) f uop

  (* Macros *)

  val var : Var.t -> [> `Var of Loc.t * Var.t]

  val product :
    Loc.t -> (Label.t * 't) list -> [> `Product of Loc.t * (Label.t * 't) list]

  val sum :
    Loc.t -> (Label.t * 't) list -> [> `Sum of Loc.t * (Label.t * 't) list]

  val zero : Loc.t -> [> `Sum of Loc.t * (Label.t * 't) list]

  (* Comparison *)

  val compare' :
    (Var.t VarMap.t -> 't cmp) -> Var.t VarMap.t -> ('t, Kind.t) f cmp

  val compare : t cmp

  (* Type predicates *)

  val is_int : t -> bool

  (* Type applications *)

  val app : Loc.t -> t -> t list -> t
  val unapp : t -> t * t list
  val arity_and_result : t -> int * t

  (* *)

  val keep_phys_eq' : ([> ('t, 'k) f] as 't) -> 't -> 't
  val keep_phys_eq : (([> ('t, 'k) f] as 't) -> 't) -> 't -> 't

  (* *)

  val impure : Var.t
  val initial_env : (Var.t * Kind.t) VarMap.t

  (* *)

  val free' : ('t -> VarSet.t) -> ('t, 'k) f -> VarSet.t
  val is_free' : (Var.t -> 't -> bool) -> Var.t -> ('t, 'k) f -> bool

  val subst_par' :
    (([> ('t, 'k) f] as 't) VarMap.t -> 't uop) ->
    (Var.t -> 't -> bool) ->
    't VarMap.t ->
    ('t, 'k) f ->
    't

  val norm' :
    ([> ('t, 'k) f] as 't) uop ->
    (Var.t -> 't -> 't -> 't) ->
    (Var.t -> 't -> bool) ->
    ('t, 'k) f ->
    't

  (* *)

  val free : t -> VarSet.t
  val is_free : Var.t -> t -> bool
  val subst : Var.t -> t -> t uop
  val subst_par : t VarMap.t -> t uop
  val subst_rec : t VarMap.t -> t uop
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
      | `LitString of JsonString.t
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
      | `OpLogicalOr
      | `OpStringCat
      | `Keep of 't
      | `Target of 't * JsonString.t ]

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

  module Var : Id.S
  module VarSet : Set.S with type elt = Var.t
  module VarMap : Map.S with type key = Var.t

  type ('e, 't, 'k) f =
    [ `Const of Loc.t * (Bigint.t, 't) Const.t
    | `Var of Loc.t * Var.t
    | `Lam of Loc.t * Var.t * 't * 'e
    | `App of Loc.t * 'e * 'e
    | `Gen of Loc.t * Typ.Var.t * 'k * 'e
    | `Inst of Loc.t * 'e * 't
    | `LetIn of Loc.t * Var.t * 'e * 'e
    | `Mu of Loc.t * 'e
    | `IfElse of Loc.t * 'e * 'e * 'e
    | `Product of Loc.t * (Label.t * 'e) list
    | `Select of Loc.t * 'e * 'e
    | `Inject of Loc.t * Label.t * 'e
    | `Case of Loc.t * 'e
    | `Pack of Loc.t * 't * 'e * 't
    | `UnpackIn of Loc.t * Typ.Var.t * Var.t * 'e * 'e ]

  type t = (t, Typ.t, Kind.t) f

  val at : ('e, 't, 'k) f -> Loc.t

  val initial_exp :
    (('e, (('t, 'k) Typ.f as 't), ('k Kind.f as 'k)) f as 'e) -> 'e
end
