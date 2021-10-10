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
  val to_string : ?numbering:Numbering.t -> t -> string
end

module Label : Id.S

module Row : sig
  type 't t = (Label.t * 't) list

  val is_tuple : 'a t -> bool
  val map : ('t -> 'u) -> 't t -> 'u t
  val map_phys_eq : 't uop -> 't t -> 't t

  (* *)

  val map_fr :
    ('a -> ('f, 'F, 'b) Applicative.fr) -> 'a t -> ('f, 'F, 'b t) Applicative.fr

  val map_phys_eq_fr :
    ('a -> ('f, 'F, 'a) Applicative.fr) -> 'a t -> ('f, 'F, 'a t) Applicative.fr
end

module Tuple : sig
  val labels : Loc.t -> 't list -> 't Row.t
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

  module Var : sig
    include Id.S

    val to_label : t -> Label.t
  end

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
    | `Product of Loc.t * 't Row.t
    | `Sum of Loc.t * 't Row.t ]

  type t = (t, Kind.t) f

  val at : ('t, 'k) f -> Loc.t
  val set_at : Loc.t -> ('t, 'k) f uop

  (* Macros *)

  val var : Var.t -> [> `Var of Loc.t * Var.t]
  val product : Loc.t -> 't Row.t -> [> `Product of Loc.t * 't Row.t]
  val sum : Loc.t -> 't Row.t -> [> `Sum of Loc.t * 't Row.t]
  val zero : Loc.t -> [> `Sum of Loc.t * 't Row.t]
  val tuple : Loc.t -> ([> `Product of Loc.t * 't Row.t] as 't) list -> 't

  val atom :
    Label.t -> [> `Sum of Loc.t * [> `Product of Loc.t * 't Row.t] Row.t]

  (* Comparison *)

  val compare' :
    (Var.t VarMap.t -> Var.t VarMap.t -> 't cmp) ->
    Var.t VarMap.t ->
    Var.t VarMap.t ->
    ('t, Kind.t) f cmp

  val compare : t cmp
  val compare_in_env : Var.t VarMap.t -> Var.t VarMap.t -> t cmp

  (* Type predicates *)

  val is_int : t -> bool

  (* Type applications *)

  val unapp : t -> t * t list
  val arity_and_result : t -> int * t

  (* *)

  val map_fr :
    ('t -> ('f, 'F, 'u) Applicative.fr) ->
    ('t, 'k) f ->
    ('f, 'F, ('u, 'k) f) Applicative.fr

  val map_eq_fr :
    ('t -> ('f, 'F, 't) Applicative.fr) ->
    ('t, 'k) f ->
    ('f, 'F, ('t, 'k) f) Applicative.fr

  val map : ('t -> 'u) -> ('t, 'k) f -> ('u, 'k) f
  val map_eq : ('t -> 't) -> ('t, 'k) f -> ('t, 'k) f
  val map_reduce : 'u bop -> 'u -> ('t -> 'u) -> ('t, 'k) f -> 'u
  val exists : ('t -> bool) -> ('t, 'k) f -> bool
  val find_map : ('t -> 'a option) -> ('t, 'k) f -> 'a option
  val eq : ('t, 'k) f bpr

  (* *)

  val keep_phys_eq' : ([> ('t, 'k) f] as 't) -> 't -> 't
  val keep_phys_eq : (([> ('t, 'k) f] as 't) -> 't) -> 't -> 't

  (* *)

  val impure : Var.t
  val initial_env : Kind.t VarMap.t

  (* *)

  val subst_rec : t VarMap.t -> t uop

  (* *)

  val free' : ('t -> VarSet.t) -> ('t, 'k) f -> VarSet.t
  val is_free' : (Var.t -> 't -> bool) -> Var.t -> ('t, 'k) f -> bool

  val subst_of_norm' :
    (([> ('t, 'k) f] as 't) VarMap.t -> 't uop) ->
    (Var.t -> 't -> bool) ->
    't VarMap.t ->
    ('t, 'k) f ->
    't

  (* *)

  val free : t -> VarSet.t
  val is_free : Var.t -> t -> bool
  val subst_of_norm : t VarMap.t -> t uop
  val mu_of_norm : Loc.t -> t uop
  val lam_of_norm : Loc.t -> Var.t -> Kind.t -> t uop
  val app_of_norm : Loc.t -> t bop
  val apps_of_norm : Loc.t -> t -> t list -> t

  (* Freshening *)

  val freshen : t -> t

  (* Formatting *)

  val hanging : t -> (document * document) option
  val pp : ?hr:bool -> ?pp_annot:(Kind.t -> document) -> t -> document
  val to_string : t -> string
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

    val map_typ_fr :
      ('t -> ('f, 'F, 'u) Applicative.fr) ->
      ('nat, 't) t ->
      ('f, 'F, ('nat, 'u) t) Applicative.fr

    (* Comparison *)

    val compare' : 'nat cmp -> 't cmp -> ('nat, 't) t cmp

    (* Constants *)

    val lit_false : ('nat, 't) t
    val lit_true : ('nat, 't) t

    (* Formatting *)

    val pp' : ('nat -> document) -> ('t -> document) -> ('nat, 't) t -> document
    val pp : (Bigint.t, Typ.t) t -> document
  end

  module Var : sig
    include Id.S

    val to_label : t -> Label.t
    val of_label : Label.t -> t
  end

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
    | `Product of Loc.t * 'e Row.t
    | `Select of Loc.t * 'e * 'e
    | `Inject of Loc.t * Label.t * 'e
    | `Case of Loc.t * 'e
    | `Pack of Loc.t * 't * 'e * 't
    | `UnpackIn of Loc.t * Typ.Var.t * 'k * Var.t * 'e * 'e ]

  type t = (t, Typ.t, Kind.t) f

  val at : ('e, 't, 'k) f -> Loc.t

  val initial_exp :
    (('e, (('t, 'k) Typ.f as 't), ('k Kind.f as 'k)) f as 'e) -> 'e

  (* *)

  val tuple : Loc.t -> ([> `Product of Loc.t * 'e Row.t] as 'e) list -> 'e

  val atom :
    Label.t -> [> `Inject of Loc.t * Label.t * [> `Product of Loc.t * 't Row.t]]

  val lit_bool : Loc.t -> bool -> ('e, 't, 'k) f
end
