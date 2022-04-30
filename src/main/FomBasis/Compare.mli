open Misc.Syntax

module Syntax : sig
  val ( <>? ) : int -> (unit -> int) -> int
  (** Composition of comparisons: [compare a b <>? fun () -> compare x y]. *)

  val ( <?> ) : 'a cmp bop
  (** Composition of comparisons: [compare_a <?> compare_b]. *)
end

val the : ('a -> 'b) -> 'b cmp -> 'a cmp

module Tuple'2 (T1 : OrderedType) (T2 : OrderedType) :
  OrderedType with type t = T1.t * T2.t

module Tuple'3 (T1 : OrderedType) (T2 : OrderedType) (T3 : OrderedType) :
  OrderedType with type t = T1.t * T2.t * T3.t

module Tuple'4
    (T1 : OrderedType)
    (T2 : OrderedType)
    (T3 : OrderedType)
    (T4 : OrderedType) : OrderedType with type t = T1.t * T2.t * T3.t * T4.t
