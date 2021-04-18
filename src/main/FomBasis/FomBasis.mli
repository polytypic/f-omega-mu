type 'a uop = 'a -> 'a
type 'a bop = 'a -> 'a -> 'a
type 'a bpr = 'a -> 'a -> bool
type 'a cmp = 'a -> 'a -> int

module Exn : sig
  val failwithf : ('a, unit, string, string, string, 'b) format6 -> 'a
  (** Fail with formatted message. *)
end

module Compare : sig
  val ( <>? ) : int -> (unit -> int) -> int
  (** Composition of comparisons: [compare a b <>? fun () -> compare x y]. *)

  val the : ('a -> 'b) -> 'b cmp -> 'a cmp

  module Pair (Lhs : Set.OrderedType) (Rhs : Set.OrderedType) :
    Set.OrderedType with type t = Lhs.t * Rhs.t
end

module Monad : sig
  module type Monad = sig
    type ('t1, 'x) t

    val return : 'x -> ('t1, 'x) t
    val ( let* ) : ('t1, 'x) t -> ('x -> ('t1, 'y) t) -> ('t1, 'y) t
  end

  module type S = sig
    include Monad

    val ( >> ) : ('r, unit) t -> ('r, 'x) t -> ('r, 'x) t

    (* *)

    val lift1 : ('d1 -> 'c) -> ('e, 'd1) t -> ('e, 'c) t
    val lift2 : ('d1 -> 'd2 -> 'c) -> ('e, 'd1) t -> ('e, 'd2) t -> ('e, 'c) t

    (* *)

    val ( &&& ) : ('e, bool) t bop
    val ( ||| ) : ('e, bool) t bop

    (* *)

    val traverse : ('x -> ('e, 'y) t) -> 'x list -> ('e, 'y list) t
    val fold_left : ('y -> 'x -> ('e, 'y) t) -> 'y -> 'x list -> ('e, 'y) t
    val iter : ('x -> ('e, unit) t) -> 'x list -> ('e, unit) t
    val for_all : ('x -> ('e, bool) t) -> 'x list -> ('e, bool) t
    val exists : ('x -> ('e, bool) t) -> 'x list -> ('e, bool) t
  end

  module Make (Core : Monad) : S with type ('t1, 'x) t = ('t1, 'x) Core.t
end

module Conser : sig
  include Monad.S with type ('r, 'a) t = 'r list -> 'a * 'r list

  val run : ('r, 'a) t -> 'a * 'r list

  (* *)

  val yield : 'r -> ('r, unit) t
end

module ListExt : sig
  val for_alli : (int -> 'a -> bool) -> 'a list -> bool
  val equal_with : 'a bpr -> 'a list bpr
  val compare_with : 'a cmp -> 'a list cmp
  val map_phys_eq : 'a uop -> 'a list uop
end

module Pair : sig
  val swap : 'a * 'b -> 'b * 'a
  (** Swap elements of a pair. *)

  val map : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
  val map_phys_eq : 'a uop -> 'b uop -> ('a * 'b) uop
end

module UTF8 : sig
  val to_uchar_array : string -> Uchar.t array
  (** Convert UTF-8 string to an array of Unicode characters. *)

  val of_uchar_array : Uchar.t array -> string
  (** Convert an array of Unicode characters to UTF-8 string. *)
end

module Reader : sig
  include Monad.S with type ('e, 'x) t = 'e -> 'x

  val run : 'e -> ('e, 'x) t -> 'x
end

val failwithf : ('a, unit, string, string, string, 'b) format6 -> 'a
(** Fail with formatted message. *)

val ( <>? ) : int -> (unit -> int) -> int
(** Composition of comparisons: [compare a b <>? fun () -> compare x y]. *)

val ( >>> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val ( <<< ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
