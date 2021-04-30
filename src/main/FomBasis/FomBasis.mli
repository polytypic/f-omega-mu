type 'a uop = 'a -> 'a
type 'a bop = 'a -> 'a -> 'a
type 'a bpr = 'a -> 'a -> bool
type 'a cmp = 'a -> 'a -> int

module Zero : sig
  type t = |
end

module Compare : sig
  val ( <>? ) : int -> (unit -> int) -> int
  (** Composition of comparisons: [compare a b <>? fun () -> compare x y]. *)

  val the : ('a -> 'b) -> 'b cmp -> 'a cmp

  module Pair (Lhs : Set.OrderedType) (Rhs : Set.OrderedType) :
    Set.OrderedType with type t = Lhs.t * Rhs.t
end

module Exn : sig
  val failwithf : ('a, unit, string, string, string, 'b) format6 -> 'a
  (** Fail with formatted message. *)
end

module Field : sig
  type ('a, 'r) t

  val make : 'a -> ('a -> 'r) -> ('a, 'r) t
  val get : ('r -> ('a, 'r) t) -> 'r -> 'a
  val map : ('r -> ('a, 'r) t) -> 'a uop -> 'r uop
  val set : ('r -> ('a, 'r) t) -> 'a -> 'r uop
end

module FilenameExt : sig
  val canonic : string -> string
end

module ListExt : sig
  val for_alli : (int -> 'a -> bool) -> 'a list -> bool
  val equal_with : 'a bpr -> 'a list bpr
  val compare_with : 'a cmp -> 'a list cmp
  val map_phys_eq : 'a uop -> 'a list uop
end

module Monad : sig
  module type Monad = sig
    type ('T1, 'T2, 'a) t

    val return : 'a -> ('T1, 'T2, 'a) t

    val ( let* ) :
      ('T1, 'T2, 'a) t -> ('a -> ('T1, 'T2, 'b) t) -> ('T1, 'T2, 'b) t

    val ( let+ ) : ('T1, 'T2, 'a) t -> ('a -> 'b) -> ('T1, 'T2, 'b) t
  end

  module type S = sig
    include Monad

    (* *)

    val ( >> ) : ('T1, 'T2, unit) t -> ('T1, 'T2, 'a) t -> ('T1, 'T2, 'a) t

    (* *)

    val lift1 : ('d1 -> 'c) -> ('T1, 'T2, 'd1) t -> ('T1, 'T2, 'c) t

    val lift2 :
      ('d1 -> 'd2 -> 'c) ->
      ('T1, 'T2, 'd1) t ->
      ('T1, 'T2, 'd2) t ->
      ('T1, 'T2, 'c) t

    (* *)

    val ( &&& ) : ('T1, 'T2, bool) t -> ('T1, 'T2, bool) t -> ('T1, 'T2, bool) t
    val ( ||| ) : ('T1, 'T2, bool) t -> ('T1, 'T2, bool) t -> ('T1, 'T2, bool) t

    (* *)

    val traverse : ('a -> ('T1, 'T2, 'b) t) -> 'a list -> ('T1, 'T2, 'b list) t

    val fold_left :
      ('b -> 'a -> ('T1, 'T2, 'b) t) -> 'b -> 'a list -> ('T1, 'T2, 'b) t

    val iter : ('a -> ('T1, 'T2, unit) t) -> 'a list -> ('T1, 'T2, unit) t
    val for_all : ('a -> ('T1, 'T2, bool) t) -> 'a list -> ('T1, 'T2, bool) t
    val exists : ('a -> ('T1, 'T2, bool) t) -> 'a list -> ('T1, 'T2, bool) t
  end

  module Make (Core : Monad) :
    S with type ('T1, 'T2, 'a) t = ('T1, 'T2, 'a) Core.t
end

module Conser : sig
  include Monad.S

  val run : ('T1, 'r, 'a) t -> 'a * 'r list

  (* *)

  val yield : 'r -> ('T1, 'r, unit) t
end

module Pair : sig
  val swap : 'a * 'b -> 'b * 'a
  (** Swap elements of a pair. *)

  val map : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
  val map_phys_eq : 'a uop -> 'b uop -> ('a * 'b) uop
end

module Rea : sig
  include Monad.S

  val start : 'r -> ('r, Zero.t, unit) t -> unit

  (* *)

  val try_in :
    ('r, 'e, 'a) t ->
    ('a -> ('r, 'f, 'b) t) ->
    ('e -> ('r, 'f, 'b) t) ->
    ('r, 'f, 'b) t

  (* *)

  val env_as : ('r -> 'a) -> ('r, 'e, 'a) t
  val with_env : ('r -> 's) -> ('s, 'e, 'a) t -> ('r, 'e, 'a) t

  (* *)

  val get : ('r -> ('f, 'r) Field.t) -> ('r, 'e, 'f) t
  val get_as : ('r -> ('f, 'r) Field.t) -> ('f -> 'g) -> ('r, 'e, 'g) t
  val setting : ('r -> ('f, 'r) Field.t) -> 'f -> ('r, 'e, 'a) t uop
  val mapping : ('r -> ('f, 'r) Field.t) -> 'f uop -> ('r, 'e, 'a) t uop
end

module Reader : sig
  include Monad.S

  val run : 'r -> ('r, 'T2, 'a) t -> 'a

  (* *)

  val env_as : ('r -> 'a) -> ('r, 'T2, 'a) t
  val with_env : ('r -> 's) -> ('s, 'T2, 'a) t -> ('r, 'T2, 'a) t

  (* *)

  val get : ('r -> ('f, 'r) Field.t) -> ('r, 'T2, 'f) t
  val get_as : ('r -> ('f, 'r) Field.t) -> ('f -> 'g) -> ('r, 'T2, 'g) t
  val setting : ('r -> ('f, 'r) Field.t) -> 'f -> ('r, 'T2, 'a) t uop
  val mapping : ('r -> ('f, 'r) Field.t) -> 'f uop -> ('r, 'T2, 'a) t uop
end

module StringExt : sig
  val is_prefix : string bpr
  val is_suffix : string bpr
  val drop : int -> string uop
  val drop_last : int -> string uop
  val split : int -> string -> string * string
  val split_on_char : char -> string -> string list
end

module UTF8 : sig
  val to_uchar_array : string -> Uchar.t array
  (** Convert UTF-8 string to an array of Unicode characters. *)

  val of_uchar_array : Uchar.t array -> string
  (** Convert an array of Unicode characters to UTF-8 string. *)
end

val failwithf : ('a, unit, string, string, string, 'b) format6 -> 'a
(** Fail with formatted message. *)

val ( <>? ) : int -> (unit -> int) -> int
(** Composition of comparisons: [compare a b <>? fun () -> compare x y]. *)

val ( >>> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val ( <<< ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
