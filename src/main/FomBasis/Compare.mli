open Misc.Syntax

module Syntax : sig
  val ( <>? ) : int -> (unit -> int) -> int
  (** Composition of comparisons: [compare a b <>? fun () -> compare x y]. *)

  val ( <?> ) : 'a cmp bop
  (** Composition of comparisons: [compare_a <?> compare_b]. *)
end

val the : ('a -> 'b) -> 'b cmp -> 'a cmp

module Tuple'2 (T1 : Set.OrderedType) (T2 : Set.OrderedType) :
  Set.OrderedType with type t = T1.t * T2.t

module Tuple'3
    (T1 : Set.OrderedType)
    (T2 : Set.OrderedType)
    (T3 : Set.OrderedType) : Set.OrderedType with type t = T1.t * T2.t * T3.t

module Tuple'4
    (T1 : Set.OrderedType)
    (T2 : Set.OrderedType)
    (T3 : Set.OrderedType)
    (T4 : Set.OrderedType) :
  Set.OrderedType with type t = T1.t * T2.t * T3.t * T4.t
