open Misc.Syntax

module type OrderedType = Stdlib.Map.OrderedType

module type S = sig
  include Stdlib.Map.S

  val add_list : (key * 'v) list -> 'v t uop
  val of_list : (key * 'v) list -> 'v t

  val exists_fr :
    (key -> 'v -> ('f, 'F, bool) Monad.fr) -> 'v t -> ('f, 'F, bool) Monad.fr
end

module Make : functor (Ord : OrderedType) -> S with type key = Ord.t

val prefer_lhs : 'k -> 'v option bop
val prefer_rhs : 'k -> 'v option bop
val combining_with : 'v bop -> 'k -> 'v option bop
