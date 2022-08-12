open Rea
open StdlibPlus
open FomSource

module Kind : sig
  module Unk : Id.S

  module Core : sig
    type 'k f = [`Star of Loc.t | `Arrow of Loc.t * 'k * 'k]
  end

  type 'k f = ['k Core.f | `Unk of Loc.t * Unk.t]
  type t = t f

  (* *)

  module UnkMap : Map.S with type key = Unk.t

  module UnkEnv : sig
    type m

    class con :
      object
        method kind_env : m
      end

    val resetting :
      ('R, 'e, 'a, (< ('R, 'D) monad' ; con ; .. > as 'D)) er ->
      ('R, 'e, 'a, 'D) er

    val find_opt :
      Unk.t -> ('R, 'e, t option, (< ('R, 'D) async' ; con ; .. > as 'D)) er

    val add :
      Unk.t -> t -> ('R, 'e, unit, (< ('R, 'D) async' ; con ; .. > as 'D)) er

    val cloning :
      ('R, 'e, 'a, (< ('R, 'D) async' ; con ; .. > as 'D)) er ->
      ('R, 'e, 'a, 'D) er
  end

  (* *)

  val at : t -> Loc.t
  val set_at : Loc.t -> t -> t

  (* *)

  val fresh : Loc.t -> t

  (* Comparison *)

  val compare : t cmp

  (* *)

  val min_arity : t -> int

  (* *)

  val keep_eq' : t -> t -> t
  val keep_eq : (t -> t) -> t -> t
  val keep_eq_er : (t -> ('R, 'e, t, (('R, 'D) #map' as 'D)) er) uop
end

module Label : sig
  include Id.S

  val begin' : t
  val finish' : t
  val string' : t
  val text' : t
end

module LabelMap : Map.S with type key = Label.t

module Row : sig
  type 't t = (Label.t * 't) list

  val is_tuple : 'a t -> bool
  val map : ('t -> 'u) -> 't t -> 'u t
  val map_eq : 't uop -> 't t -> 't t

  (* *)

  val union_er :
    (Label.t -> 'a -> ('R, 'e, 'c, (('R, 'D) #applicative' as 'D)) er) ->
    (Label.t -> 'b -> ('R, 'e, 'c, 'D) er) ->
    (Label.t -> 'a -> 'b -> ('R, 'e, 'c, 'D) er) ->
    'a t ->
    'b t ->
    ('R, 'e, 'c t, 'D) er

  val map_er :
    ('a -> ('R, 'e, 'b, (('R, 'D) #applicative' as 'D)) er) ->
    'a t ->
    ('R, 'e, 'b t, 'D) er

  val map_eq_er :
    ('a -> ('R, 'e, 'a, (('R, 'D) #applicative' as 'D)) er) ->
    'a t ->
    ('R, 'e, 'a t, 'D) er
end

module Tuple : sig
  val labels : Loc.t -> 't list -> 't Row.t
end

module Typ : sig
  module Const : sig
    type t = [`Bool | `Int | `String | `Unit]

    (* Comparison *)

    val equal : t bpr
    val compare : t cmp

    (* Kinding *)

    val kind_of : Loc.t -> t -> Kind.t
  end

  module Var : sig
    include Id.S

    val to_label : t -> Label.t
  end

  module VarSet : Set.S with type elt = Var.t
  module VarMap : Map.S with type key = Var.t

  module Core : sig
    type ('t, 'k) f =
      [ `Mu of Loc.t * 't
      | `Const of Loc.t * Const.t
      | `Var of Loc.t * Var.t
      | `Lam of Loc.t * Var.t * 'k * 't
      | `App of Loc.t * 't * 't
      | `Arrow of Loc.t * 't * 't
      | `For of Loc.t * [`All | `Unk] * 't
      | `Row of Loc.t * [`Product | `Sum] * 't Row.t ]

    type t = (t, Kind.t) f

    val set_at : Loc.t -> [< ('t, 'k) f] -> ('t, 'k) f

    (* *)

    val map_er :
      ('t -> ('R, 'e, 'u, (('R, 'D) #applicative' as 'D)) er) ->
      ('t, 'k) f ->
      ('R, 'e, ('u, 'k) f, 'D) er

    val map_eq_er :
      ('t -> ('R, 'e, 't, (('R, 'D) #applicative' as 'D)) er) ->
      ('t, 'k) f ->
      ('R, 'e, ('t, 'k) f, 'D) er

    val map : ('t -> 'u) -> ('t, 'k) f -> ('u, 'k) f
    val map_eq : ('t -> 't) -> ('t, 'k) f -> ('t, 'k) f
    val map_reduce : 'a lazy_op'2 -> 'a -> ('t -> 'a) -> ('t, 'k) f -> 'a
    val exists : ('t -> bool) -> ('t, 'k) f -> bool

    val exists_er :
      ('t -> (('R, 'e, bool, (('R, 'D) #monad' as 'D)) er as 'E)) ->
      ('t, 'k) f ->
      'E

    val find_map : ('t -> 'a option) -> ('t, 'k) f -> 'a option

    val iter_er :
      ('t -> (('R, 'e, unit, (('R, 'D) #monad' as 'D)) er as 'E)) ->
      ('t, 'k) f ->
      'E

    (* *)

    val eq : ('t, 'k) f bpr

    (* *)

    val keep_eq' : ([> ('t, 'k) f] as 't) -> 't -> 't
    val keep_eq : (([> ('t, 'k) f] as 't) -> 't) -> 't -> 't

    val keep_eq_er :
      (([> ('t, 'k) f] as 't) -> ('R, 'e, 't, (('R, 'D) #map' as 'D)) er) uop

    (* *)

    val is_free : Var.t -> t -> ('R, 'e, bool, (('R, 'D) #monad' as 'D)) er

    val subst_of_norm :
      Var.t -> t -> t -> ('R, 'e, t, (('R, 'D) #monad' as 'D)) er

    val mu_of_norm : Loc.t -> t -> ('R, 'e, t, (('R, 'D) #monad' as 'D)) er

    val lam_of_norm :
      Loc.t -> Var.t -> Kind.t -> t -> ('R, 'e, t, (('R, 'D) #monad' as 'D)) er

    val app_of_norm :
      Loc.t -> t -> t -> ('R, 'e, t, (('R, 'D) #monad' as 'D)) er

    val apps_of_norm :
      Loc.t -> t -> t list -> ('R, 'e, t, (('R, 'D) #monad' as 'D)) er
  end

  type ('t, 'k) f =
    [('t, 'k) Core.f | `Bop of Loc.t * [`Join | `Meet | `Eq] * 't * 't]

  type t = (t, Kind.t) f

  val at : [< ('t, 'k) f] -> Loc.t
  val set_at : Loc.t -> [< ('t, 'k) f] -> ('t, 'k) f

  (* Macros *)

  val var : Var.t -> [> `Var of Loc.t * Var.t]
  val product : Loc.t -> 't Row.t -> [> `Row of Loc.t * [> `Product] * 't Row.t]
  val sum : Loc.t -> 't Row.t -> [> `Row of Loc.t * [> `Sum] * 't Row.t]
  val zero : Loc.t -> [> `Row of Loc.t * [> `Sum] * 't Row.t]

  val tuple :
    Loc.t ->
    ([> `Const of Loc.t * [> `Unit] | `Row of Loc.t * [> `Product] * 't Row.t]
     as
     't)
    list ->
    't

  val atom :
    Label.t ->
    [> `Row of Loc.t * [> `Sum] * [> `Const of Loc.t * [> `Unit]] Row.t]

  (* Comparison *)

  val compare' :
    (Var.t VarMap.t -> Var.t VarMap.t -> 't cmp) ->
    Var.t VarMap.t ->
    Var.t VarMap.t ->
    ('t, Kind.t) f cmp

  val compare : t cmp
  val compare_in_env : Var.t VarMap.t -> Var.t VarMap.t -> t cmp

  (* Type predicates *)

  val is_int : [> ('t, 'k) Core.f] -> bool

  (* Type applications *)

  val unlam :
    ([> `Lam of Loc.t * Var.t * 'k * 't] as 't) -> 't * (Var.t * 'k) list

  val unapp : ([> `App of Loc.t * 't * 't] as 't) -> 't * 't list
  val arity_and_result : ([> `Arrow of Loc.t * 't * 't] as 't) -> int * 't

  (* *)

  val map_er :
    ('t -> ('R, 'e, 'u, (('R, 'D) #applicative' as 'D)) er) ->
    ('t, 'k) f ->
    ('R, 'e, ('u, 'k) f, 'D) er

  val map_eq_er :
    ('t -> ('R, 'e, 't, (('R, 'D) #applicative' as 'D)) er) ->
    ('t, 'k) f ->
    ('R, 'e, ('t, 'k) f, 'D) er

  val map : ('t -> 'u) -> ('t, 'k) f -> ('u, 'k) f
  val map_eq : ('t -> 't) -> ('t, 'k) f -> ('t, 'k) f
  val map_reduce : 'u lazy_op'2 -> 'u -> ('t -> 'u) -> ('t, 'k) f -> 'u
  val exists : ('t -> bool) -> ('t, 'k) f -> bool

  val exists_er :
    ('t -> (('R, 'e, bool, (('R, 'D) #monad' as 'D)) er as 'E)) ->
    ('t, 'k) f ->
    'E

  val find_map : ('t -> 'a option) -> ('t, 'k) f -> 'a option

  val find_map_er :
    ('t -> (('R, 'e, 'a option, (('R, 'D) #monad' as 'D)) er as 'E)) ->
    ('t, 'k) f ->
    'E

  (* *)

  val eq : ('t, 'k) f bpr

  (* *)

  val keep_eq' : ([> ('t, 'k) f] as 't) -> 't -> 't
  val keep_eq : (([> ('t, 'k) f] as 't) -> 't) -> 't -> 't

  val keep_eq_er :
    (([> ('t, 'k) f] as 't) -> ('R, 'e, 't, (('R, 'D) #map' as 'D)) er) uop

  (* *)

  val impure : Var.t
  val initial_env : [`Kind of Kind.t | `Typ of t] VarMap.t

  (* *)

  val subst_rec : t VarMap.t -> t uop

  (* *)

  val is_free : Var.t -> t -> ('R, 'e, bool, (('R, 'D) #monad' as 'D)) er

  (* Freshening *)

  val freshen : t -> t
end

module Exp : sig
  module Const : sig
    type ('nat, 't) t =
      [ `Bool of bool
      | `Nat of 'nat
      | `String of JsonString.t
      | `Unit
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

    val type_of : Loc.t -> ('nat, ([> ('t, 'k) Typ.Core.f] as 't)) t -> 't

    (* Substitution *)

    val map_typ_er :
      ('t -> ('R, 'e, 'u, (('R, 'D) #applicative' as 'D)) er) ->
      ('nat, 't) t ->
      ('R, 'e, ('nat, 'u) t, 'D) er

    (* Comparison *)

    val compare' : 'nat cmp -> 't cmp -> ('nat, 't) t cmp

    (* Constants *)

    val lit_false : ('nat, 't) t
    val lit_true : ('nat, 't) t
  end

  module Var : sig
    include Id.S

    val to_label : t -> Label.t
    val of_label : Label.t -> t
  end

  module VarSet : Set.S with type elt = Var.t
  module VarMap : Map.S with type key = Var.t

  module Core : sig
    type ('e, 't, 'k) f =
      [ `Const of Loc.t * (Bigint.t, 't) Const.t
      | `Var of Loc.t * Var.t
      | `Lam of Loc.t * Var.t * 't * 'e
      | `App of Loc.t * 'e * 'e
      | `Gen of Loc.t * Typ.Var.t * 'k * 'e
      | `Inst of Loc.t * 'e * 't
      | `Mu of Loc.t * 'e
      | `IfElse of Loc.t * 'e * 'e * 'e
      | `Product of Loc.t * 'e Row.t
      | `Select of Loc.t * 'e * 'e
      | `Inject of Loc.t * Label.t * 'e
      | `Case of Loc.t * 'e
      | `Pack of Loc.t * 't * 'e * 't
      | `UnpackIn of Loc.t * Typ.Var.t * 'k * Var.t * 'e * 'e ]

    type t = (t, Typ.Core.t, Kind.t) f
  end

  type ('e, 't, 'k) f =
    [ ('e, 't, 'k) Core.f
    | `LamImp of Loc.t * Var.t * 'e
    | `PackImp of Loc.t * 't * 'e
    | `Merge of Loc.t * 'e * 'e ]

  type t = (t, Typ.t, Kind.t) f

  val at : [< ('e, 't, 'k) f] -> Loc.t

  (* *)

  val raw : Var.t

  val initial_exp :
    (('e, (('t, 'k) Typ.f as 't), ('k Kind.f as 'k)) f as 'e) -> 'e

  (* *)

  val var : Var.t -> [> `Var of Loc.t * Var.t]

  val tuple :
    Loc.t ->
    ([> `Const of Loc.t * [> `Unit] | `Product of Loc.t * 'e Row.t] as 'e) list ->
    'e

  val product : Loc.t -> 'e Row.t -> [> `Product of Loc.t * 'e Row.t]

  val atom :
    Label.t -> [> `Inject of Loc.t * Label.t * [> `Const of Loc.t * [> `Unit]]]

  val lit_bool : Loc.t -> bool -> ('e, 't, 'k) f
end
