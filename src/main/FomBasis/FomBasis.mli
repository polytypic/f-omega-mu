type 'a uop = 'a -> 'a
type 'a bop = 'a -> 'a -> 'a
type 'a bpr = 'a -> 'a -> bool
type 'a cmp = 'a -> 'a -> int

module Res : sig
  type ('e, 'a) t = [`Ok of 'a | `Error of 'e]
end

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
    type (-'I, 'T, +'O, +'a) m

    val return : 'a -> ('I, 'T, 'O, 'a) m

    val ( let* ) :
      ('I, 'T, 'O, 'a) m -> ('a -> ('I, 'T, 'O, 'b) m) -> ('I, 'T, 'O, 'b) m

    val ( let+ ) : ('I, 'T, 'O, 'a) m -> ('a -> 'b) -> ('I, 'T, 'O, 'b) m

    val ( and* ) :
      ('I, 'T, 'O, 'a) m -> ('I, 'T, 'O, 'b) m -> ('I, 'T, 'O, 'a * 'b) m
  end

  module type S = sig
    include Monad

    val ( and+ ) :
      ('I, 'T, 'O, 'a) m -> ('I, 'T, 'O, 'b) m -> ('I, 'T, 'O, 'a * 'b) m

    (* *)

    val ( >> ) :
      ('I, 'T, 'O, unit) m -> ('I, 'T, 'O, 'a) m -> ('I, 'T, 'O, 'a) m

    val ( >>= ) :
      ('I, 'T, 'O, 'a) m -> ('a -> ('I, 'T, 'O, 'b) m) -> ('I, 'T, 'O, 'b) m

    val ( >>- ) : ('I, 'T, 'O, 'a) m -> ('a -> 'b) -> ('I, 'T, 'O, 'b) m

    (* *)

    val lift1 : ('d1 -> 'c) -> ('I, 'T, 'O, 'd1) m -> ('I, 'T, 'O, 'c) m

    val lift2 :
      ('d1 -> 'd2 -> 'c) ->
      ('I, 'T, 'O, 'd1) m ->
      ('I, 'T, 'O, 'd2) m ->
      ('I, 'T, 'O, 'c) m

    (* *)

    val ( &&& ) :
      ('I, 'T, 'O, bool) m -> ('I, 'T, 'O, bool) m -> ('I, 'T, 'O, bool) m

    val ( ||| ) :
      ('I, 'T, 'O, bool) m -> ('I, 'T, 'O, bool) m -> ('I, 'T, 'O, bool) m

    (* *)

    module MList : sig
      val fold_left :
        ('a -> 'b -> ('I, 'T, 'O, 'a) m) -> 'a -> 'b list -> ('I, 'T, 'O, 'a) m

      val fold_left2 :
        ('a -> 'b -> 'c -> ('I, 'T, 'O, 'a) m) ->
        'a ->
        'b list ->
        'c list ->
        ('I, 'T, 'O, 'a) m

      (* *)

      val iter : ('a -> ('I, 'T, 'O, unit) m) -> 'a list -> ('I, 'T, 'O, unit) m

      val iter2 :
        ('a -> 'b -> ('I, 'T, 'O, unit) m) ->
        'a list ->
        'b list ->
        ('I, 'T, 'O, unit) m

      (* *)

      val for_all :
        ('a -> ('I, 'T, 'O, bool) m) -> 'a list -> ('I, 'T, 'O, bool) m

      val exists :
        ('a -> ('I, 'T, 'O, bool) m) -> 'a list -> ('I, 'T, 'O, bool) m

      (* *)

      val traverse :
        ('a -> ('I, 'T, 'O, 'b) m) -> 'a list -> ('I, 'T, 'O, 'b list) m
    end

    module MOption : sig
      val iter :
        ('a -> ('I, 'T, 'O, unit) m) -> 'a option -> ('I, 'T, 'O, unit) m

      val traverse :
        ('a -> ('I, 'T, 'O, 'b) m) -> 'a option -> ('I, 'T, 'O, 'b option) m
    end
  end

  module Make (Core : Monad) :
    S with type ('I, 'T, 'O, 'a) m = ('I, 'T, 'O, 'a) Core.m
end

module Pair : sig
  val swap : 'a * 'b -> 'b * 'a
  (** Swap elements of a pair. *)

  val map : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
  val map_phys_eq : 'a uop -> 'b uop -> ('a * 'b) uop
end

module Rea : sig
  type (-'r, +'e, +'a) t

  include Monad.S with type ('r, 'T, 'e, 'a) m = ('r, 'e, 'a) t

  val unit : ('r, 'e, unit) t
  val delay : (unit -> ('r, 'e, 'a) t) -> ('r, 'e, 'a) t

  (* *)

  val start : 'r -> ('r, Zero.t, unit) t -> unit

  (* *)

  val of_async : ('r -> ('e -> unit) -> ('a -> unit) -> unit) -> ('r, 'e, 'a) t
  val of_res : ('e, 'a) Res.t -> ('r, 'e, 'a) t

  (* *)

  val fail : 'e -> ('r, 'e, 'a) t

  (* *)

  val try_in :
    ('a -> ('r, 'f, 'b) t) ->
    ('e -> ('r, 'f, 'b) t) ->
    ('r, 'e, 'a) t ->
    ('r, 'f, 'b) t

  val catch : ('r, 'e, 'a) t -> ('r, 'f, ('e, 'a) Res.t) t

  (* *)

  val map_error : ('e -> 'f) -> ('r, 'e, 'a) t -> ('r, 'f, 'a) t

  (* *)

  val env_as : ('r -> 'a) -> ('r, 'e, 'a) t
  val with_env : ('r -> 's) -> ('s, 'e, 'a) t -> ('r, 'e, 'a) t

  (* *)

  val invoke : ('r -> (unit, 'e, 'a) t) -> ('r, 'e, 'a) t

  (* *)

  val get : ('r -> ('f, 'r) Field.t) -> ('r, 'e, 'f) t
  val get_as : ('r -> ('f, 'r) Field.t) -> ('f -> 'g) -> ('r, 'e, 'g) t
  val setting : ('r -> ('f, 'r) Field.t) -> 'f -> ('r, 'e, 'a) t uop
  val mapping : ('r -> ('f, 'r) Field.t) -> 'f uop -> ('r, 'e, 'a) t uop
end

module IVar : sig
  type ('e, 'a) t

  val empty : unit -> ('e, 'a) t
  val get : ('e, 'a) t -> ('r, 'e, 'a) Rea.t
  val put : ('e, 'a) t -> [`Ok of 'a | `Error of 'e] -> ('r, 'f, unit) Rea.t
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
