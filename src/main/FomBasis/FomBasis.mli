module Exn : sig
  val failwithf : ('a, unit, string, string, string, 'b) format6 -> 'a
  (** Fail with formatted message. *)
end

module Compare : sig
  val ( <>? ) : int -> (unit -> int) -> int
  (** Composition of comparisons: [compare a b <>? fun () -> compare x y]. *)

  val the : ('a -> 'b) -> ('b -> 'b -> int) -> 'a -> 'a -> int

  module Pair (Lhs : Set.OrderedType) (Rhs : Set.OrderedType) :
    Set.OrderedType with type t = Lhs.t * Rhs.t
end

module ListExt : sig
  val for_alli : (int -> 'a -> bool) -> 'a list -> bool
  val equal_with : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
  val compare_with : ('a -> 'a -> int) -> 'a list -> 'a list -> int
  val map_phys_eq : ('a -> 'a) -> 'a list -> 'a list
end

module Pair : sig
  val swap : 'a * 'b -> 'b * 'a
  (** Swap elements of a pair. *)

  val map : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
  val map_phys_eq : ('a -> 'a) -> ('b -> 'b) -> 'a * 'b -> 'a * 'b
end

module UTF8 : sig
  val to_uchar_array : string -> Uchar.t array
  (** Convert UTF-8 string to an array of Unicode characters. *)

  val of_uchar_array : Uchar.t array -> string
  (** Convert an array of Unicode characters to UTF-8 string. *)
end

module Reader : sig
  type ('e, 'x) t = 'e -> 'x

  val run : 'e -> ('e, 'x) t -> 'x

  (* *)

  val return : 'x -> ('e, 'x) t
  val ( let* ) : ('e, 'x) t -> ('x -> ('e, 'y) t) -> ('e, 'y) t

  (* *)

  val ( &&& ) : ('e, bool) t -> ('e, bool) t -> ('e, bool) t
  val ( ||| ) : ('e, bool) t -> ('e, bool) t -> ('e, bool) t

  (* *)

  val lift1 : ('d1 -> 'c) -> ('e, 'd1) t -> ('e, 'c) t
  val lift2 : ('d1 -> 'd2 -> 'c) -> ('e, 'd1) t -> ('e, 'd2) t -> ('e, 'c) t

  (* *)

  val traverse : ('x -> ('e, 'y) t) -> 'x list -> ('e, 'y list) t
  val fold_left : ('y -> 'x -> ('e, 'y) t) -> 'y -> 'x list -> ('e, 'y) t
  val iter : ('x -> ('e, unit) t) -> 'x list -> ('e, unit) t
  val for_all : ('x -> ('e, bool) t) -> 'x list -> ('e, bool) t
  val exists : ('x -> ('e, bool) t) -> 'x list -> ('e, bool) t
end

val id : 'a -> 'a
(** Identity function. *)

val failwithf : ('a, unit, string, string, string, 'b) format6 -> 'a
(** Fail with formatted message. *)

val ( <>? ) : int -> (unit -> int) -> int
(** Composition of comparisons: [compare a b <>? fun () -> compare x y]. *)

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val ( << ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
