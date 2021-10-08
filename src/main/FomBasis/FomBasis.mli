type 'a uop = 'a -> 'a
type 'a bop = 'a -> 'a -> 'a
type 'a bpr = 'a -> 'a -> bool
type 'a cmp = 'a -> 'a -> int

module Higher : sig
  type ('x, 'f) t

  module Syntax : sig
    type ('x, 'f) app'1 = ('x, 'f) t
    type ('x, 'y, 'f) app'2 = ('y, ('x, 'f) t) t
    type ('x, 'y, 'z, 'f) app'3 = ('z, ('y, ('x, 'f) t) t) t
  end

  (* *)

  module type T'1 = sig
    type 'a t
  end

  module type F'1 = sig
    type f
    type 'a t'1

    val inj : 'a t'1 -> ('a, f) Syntax.app'1
    val prj : ('a, f) Syntax.app'1 -> 'a t'1
  end

  module New'1 : functor (T'1 : T'1) () -> F'1 with type 'a t'1 = 'a T'1.t

  (* *)

  module type T'2 = sig
    type ('a, 'b) t
  end

  module type F'2 = sig
    type f
    type ('a, 'b) t'2

    val inj : ('a, 'b) t'2 -> ('a, 'b, f) Syntax.app'2
    val prj : ('a, 'b, f) Syntax.app'2 -> ('a, 'b) t'2
  end

  module New'2 (T'2 : T'2) () : F'2 with type ('a, 'b) t'2 = ('a, 'b) T'2.t

  (* *)

  module type T'3 = sig
    type ('a, 'b, 'c) t
  end

  module type F'3 = sig
    type f
    type ('a, 'b, 'c) t'3

    val inj : ('a, 'b, 'c) t'3 -> ('a, 'b, 'c, f) Syntax.app'3
    val prj : ('a, 'b, 'c, f) Syntax.app'3 -> ('a, 'b, 'c) t'3
  end

  module New'3 : functor (T'3 : T'3) () ->
    F'3 with type ('a, 'b, 'c) t'3 = ('a, 'b, 'c) T'3.t
end

include module type of Higher.Syntax

module Functor : sig
  type ('a, 'b, 'f) map = ('a -> 'b) -> ('a, 'f) app'1 -> ('b, 'f) app'1
  type 'f t = < map : 'a 'b. ('a, 'b, 'f) map >
  type ('f, 'F, 'a) fr = (< 'f t ; .. > as 'F) -> ('a, 'f) app'1

  module Syntax : sig
    val ( let+ ) : ('f, 'F, 'a) fr -> ('a -> 'b) -> ('f, 'F, 'b) fr

    (* *)

    val ( >>- ) : ('f, 'F, 'a) fr -> ('a -> 'b) -> ('f, 'F, 'b) fr
    val ( >-> ) : ('a -> ('f, 'F, 'b) fr) -> ('b -> 'c) -> 'a -> ('f, 'F, 'c) fr

    (* *)

    val lift1 : ('a -> 'b) -> ('f, 'F, 'a) fr -> ('f, 'F, 'b) fr
  end
end

include module type of Functor.Syntax

module Applicative : sig
  type ('a, 'f) return = 'a -> ('a, 'f) app'1

  type ('a, 'b, 'f) pair =
    ('a, 'f) app'1 -> ('b, 'f) app'1 -> ('a * 'b, 'f) app'1

  type 'f t =
    < 'f Functor.t
    ; return : 'a. ('a, 'f) return
    ; pair : 'a 'b. ('a, 'b, 'f) pair >

  type ('f, 'F, 'a) fr = (< 'f t ; .. > as 'F) -> ('a, 'f) app'1

  module Syntax : sig
    val return : 'a -> ('f, 'F, 'a) fr
    val ( and+ ) : ('f, 'F, 'a) fr -> ('f, 'F, 'b) fr -> ('f, 'F, 'a * 'b) fr

    (* *)

    val ( <*> ) : ('f, 'F, 'a) fr -> ('f, 'F, 'b) fr -> ('f, 'F, 'a * 'b) fr

    (* *)

    val unit : ('f, 'F, unit) fr
    val if_then : bool -> ('f, 'F, unit) fr -> ('f, 'F, unit) fr

    (* *)

    val lift2 :
      ('a -> 'b -> 'c) -> ('f, 'F, 'a) fr -> ('f, 'F, 'b) fr -> ('f, 'F, 'c) fr
  end
end

include module type of Applicative.Syntax

module Monad : sig
  type ('a, 'b, 'f) bind =
    ('a -> ('b, 'f) app'1) -> ('a, 'f) app'1 -> ('b, 'f) app'1

  type 'f t = < 'f Applicative.t ; bind : 'a 'b. ('a, 'b, 'f) bind >
  type ('f, 'F, 'a) fr = (< 'f t ; .. > as 'F) -> ('a, 'f) app'1

  module Syntax : sig
    val ( let* ) : ('f, 'F, 'a) fr -> ('a -> ('f, 'F, 'b) fr) -> ('f, 'F, 'b) fr
    val ( and* ) : ('f, 'F, 'a) fr -> ('f, 'F, 'b) fr -> ('f, 'F, 'a * 'b) fr

    (* *)

    val ( >>= ) : ('f, 'F, 'a) fr -> ('a -> ('f, 'F, 'b) fr) -> ('f, 'F, 'b) fr
    val ( >> ) : ('f, 'F, unit) fr -> ('f, 'F, 'a) fr -> ('f, 'F, 'a) fr

    val ( >=> ) :
      ('a -> ('f, 'F, 'b) fr) ->
      ('b -> ('f, 'F, 'c) fr) ->
      'a ->
      ('f, 'F, 'c) fr

    (* *)

    val ( ||| ) : ('f, 'F, bool) fr bop
    val ( &&& ) : ('f, 'F, bool) fr bop

    (* *)

    val delay : (unit -> ('f, 'F, 'a) fr) -> ('f, 'F, 'a) fr
  end
end

include module type of Monad.Syntax

module Identity : sig
  type 'a t = 'a

  include Higher.F'1 with type 'a t'1 = 'a

  type 'a fr = f Monad.t -> ('a, f) app'1

  val inj'1 : ('a -> 'b t) -> 'a -> 'b fr
  val run : 'a fr -> 'a
end

module Monoid : sig
  type 'a t = < identity : 'a ; combine : 'a bop >
end

module Constant : sig
  type ('c, 'a) t

  include Higher.F'2 with type ('c, 'a) t'2 = ('c, 'a) t

  val ( let+ ) : ('a, 'b, ('c, f) app'1) Functor.map

  (* *)

  val from : 'c -> ('c, 'a, f) app'2
  val eval : ('c, 'a, f) app'2 -> 'c

  (* *)

  val inj'0 : 'c -> (('c, f) app'1, 'F, 'a) Functor.fr
  val inj'1 : ('x -> 'c) -> 'x -> (('c, f) app'1, 'F, 'a) Functor.fr

  (* *)

  val of_monoid : < 'c Monoid.t ; .. > -> ('c, f) app'1 Applicative.t

  (* *)

  val or_lm : (bool Lazy.t, f) app'1 Applicative.t
  val option_lm : ('a Option.t Lazy.t, f) app'1 Applicative.t
end

module Res : sig
  type ('e, 'a) t = [`Ok of 'a | `Error of 'e]
end

module Zero : sig
  type t = |
end

module Cats : sig
  type t

  val str : string -> t
  val ( ^ ) : t -> t -> t
  val to_string : t -> string
end

module Compare : sig
  module Syntax : sig
    val ( <>? ) : int -> (unit -> int) -> int
    (** Composition of comparisons: [compare a b <>? fun () -> compare x y]. *)
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
end

include module type of Compare.Syntax

module Exn : sig
  module Syntax : sig
    val failwithf : ('a, unit, string, string, string, 'b) format6 -> 'a
    (** Fail with formatted message. *)
  end
end

include module type of Exn.Syntax

module Field : sig
  type ('a, 'r) t

  val make : 'a -> ('a -> 'r) -> ('a, 'r) t
  val get : ('r -> ('a, 'r) t) -> 'r -> 'a
  val map : ('r -> ('a, 'r) t) -> 'a uop -> 'r uop
  val set : ('r -> ('a, 'r) t) -> 'a -> 'r uop
end

module Filename : sig
  include module type of Stdlib.Filename

  val canonic : string -> string
end

module Fun : sig
  include module type of Stdlib.Fun

  module Syntax : sig
    val id : 'a -> 'a
    val const : 'a -> 'b -> 'a
    val ( >>> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
    val ( <<< ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  end
end

include module type of Fun.Syntax

module JsonString : sig
  include Map.OrderedType

  val of_utf8_json : string -> t
  val to_utf8_json : t -> string

  (* *)

  val of_utf8 : string -> t
  val to_utf8 : t -> string
end

module List : sig
  include module type of Stdlib.List

  val for_alli : (int -> 'a -> bool) -> 'a list -> bool
  val equal_with : 'a bpr -> 'a list bpr
  val compare_with : 'a cmp -> 'a list cmp
  val find_dup_opt : 'a cmp -> 'a list -> ('a * 'a) option

  (* *)

  val map_phys_eq : 'a uop -> 'a list uop
  val share_phys_eq : 'a bop -> 'a list bop

  (* *)

  val fold_left_fr :
    ('a -> 'b -> ('f, 'F, 'a) Monad.fr) -> 'a -> 'b t -> ('f, 'F, 'a) Monad.fr

  val fold_left2_fr :
    ('a -> 'b -> 'c -> ('f, 'F, 'a) Monad.fr) ->
    'a ->
    'b t ->
    'c t ->
    ('f, 'F, 'a) Monad.fr

  (* *)

  val iter_fr :
    ('a -> ('f, 'F, unit) Monad.fr) -> 'a t -> ('f, 'F, unit) Monad.fr

  val iter2_fr :
    ('a -> 'b -> ('f, 'F, unit) Monad.fr) ->
    'a t ->
    'b t ->
    ('f, 'F, unit) Monad.fr

  (* *)

  val for_all_fr :
    ('a -> ('f, 'F, bool) Monad.fr) -> 'a t -> ('f, 'F, bool) Monad.fr

  (* *)

  val find_opt_fr :
    ('a -> ('f, 'F, bool) Monad.fr) -> 'a t -> ('f, 'F, 'a option) Monad.fr

  (* *)

  val map_fr :
    ('a -> ('f, 'F, 'b) Applicative.fr) -> 'a t -> ('f, 'F, 'b t) Applicative.fr

  val map_phys_eq_fr :
    ('a -> ('f, 'F, 'a) Applicative.fr) -> 'a t -> ('f, 'F, 'a t) Applicative.fr
end

module Map : sig
  module type OrderedType = Stdlib.Map.OrderedType

  module type S = sig
    include Stdlib.Map.S

    val add_list : (key * 'v) list -> 'v t uop
    val of_list : (key * 'v) list -> 'v t
  end

  module Make : functor (Ord : OrderedType) -> S with type key = Ord.t

  val prefer_lhs : 'k -> 'v option bop
  val prefer_rhs : 'k -> 'v option bop
end

module Option : sig
  include module type of Stdlib.Option

  val iter_fr :
    ('a -> ('f, 'F, unit) Applicative.fr) ->
    'a t ->
    ('f, 'F, unit) Applicative.fr
end

module Pair : sig
  val swap : 'a * 'b -> 'b * 'a
  (** Swap elements of a pair. *)

  val map : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
  val map_phys_eq : 'a uop -> 'b uop -> ('a * 'b) uop
  val share_phys_eq : 'a bop -> 'b bop -> ('a * 'b) bop

  (* *)

  val map_fr :
    ('a -> ('f, 'F, 'b) Applicative.fr) ->
    ('c -> ('f, 'F, 'd) Applicative.fr) ->
    'a * 'c ->
    ('f, 'F, 'b * 'd) Applicative.fr

  val map_phys_eq_fr :
    ('a -> ('f, 'F, 'a) Applicative.fr) ->
    ('b -> ('f, 'F, 'b) Applicative.fr) ->
    'a * 'b ->
    ('f, 'F, 'a * 'b) Applicative.fr
end

module Rea : sig
  type (-'r, +'e, +'a) t

  include Higher.F'3 with type ('r, 'e, 'a) t'3 = ('r, 'e, 'a) t

  type ('r, 'e, 'a) fr = ('r, 'e, f) app'2 Monad.t -> ('r, 'e, 'a, f) app'3

  module Syntax : sig
    type ('r, 'e, 'a) rea = ('r, 'e, 'a) fr

    val start : 'r -> ('r, Zero.t, unit) fr -> unit

    (* *)

    val of_async :
      ('r -> ('e -> unit) -> ('a -> unit) -> unit) -> ('r, 'e, 'a) rea

    val of_res : ('e, 'a) Res.t -> ('r, 'e, 'a) rea

    (* *)

    val fail : 'e -> ('r, 'e, 'a) rea

    (* *)

    val try_in :
      ('a -> ('r, 'f, 'b) rea) ->
      ('e -> ('r, 'f, 'b) rea) ->
      ('r, 'e, 'a) rea ->
      ('r, 'f, 'b) rea

    val catch : ('r, 'e, 'a) rea -> ('r, 'e, ('e, 'a) Res.t) rea

    (* *)

    val map_error : ('e -> 'f) -> ('r, 'e, 'a) rea -> ('r, 'f, 'a) rea
    val generalize_error : ('r, Zero.t, 'a) rea -> ('r, 'e, 'a) rea

    (* *)

    val env_as : ('r -> 'a) -> ('r, 'e, 'a) rea
    val with_env : ('r -> 's) -> ('s, 'e, 'a) rea -> ('r, 'e, 'a) rea
    val replace_env : 's -> ('s, 'e, 'a) rea -> ('r, 'e, 'a) rea

    (* *)

    val invoke : ('r -> (unit, 'e, 'a) rea) -> ('r, 'e, 'a) rea

    (* *)

    val get : ('r -> ('f, 'r) Field.t) -> ('r, 'e, 'f) rea
    val get_as : ('r -> ('f, 'r) Field.t) -> ('f -> 'g) -> ('r, 'e, 'g) rea
    val setting : ('r -> ('f, 'r) Field.t) -> 'f -> ('r, 'e, 'a) rea uop
    val mapping : ('r -> ('f, 'r) Field.t) -> 'f uop -> ('r, 'e, 'a) rea uop

    module IVar : sig
      type ('e, 'a) t

      val empty : unit -> ('e, 'a) t
      val get : ('e, 'a) t -> ('r, 'e, 'a) rea
      val put : ('e, 'a) t -> ('e, 'a) Res.t -> ('r, 'f, unit) rea
    end

    module LVar : sig
      type ('e, 'a) t

      val create : ('r, 'e, 'a) rea -> ('r, 'f, ('e, 'a) t) rea
      val get : ('e, 'a) t -> ('r, 'e, 'a) rea
    end

    module MVar : sig
      type 'v t

      val create : 'v -> 'v t
      val get : 'v t -> ('r, 'e, 'v) rea
      val mutate : 'v t -> ('v -> 'v) -> ('r, 'e, unit) rea
      val try_mutate : 'v t -> ('v -> ('r, 'e, 'v) rea) -> ('r, 'e, unit) rea
      val try_modify : 'v t -> ('v -> ('r, 'e, 'v * 'a) rea) -> ('r, 'e, 'a) rea
    end
  end
end

include module type of Rea.Syntax

module Profiling : sig
  module Counter : sig
    type t

    val register : string -> t
    val inc : t -> unit
    val reset_all : unit -> unit
    val dump_all : unit -> unit

    (* *)

    val wrap'1 : string -> ('a -> 'b) uop
    val wrap'2 : string -> ('a -> 'b -> 'c) uop
    val wrap'3 : string -> ('a -> 'b -> 'c -> 'd) uop
    val wrap'4 : string -> ('a -> 'b -> 'c -> 'd -> 'e) uop
  end
end

module String : sig
  include module type of Stdlib.String

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

module UTF16 : sig
  val of_uchar_array : Uchar.t array -> bytes
end

(* *)

val eq'2 : ('a * 'b) bpr
val eq'3 : ('a * 'b * 'c) bpr
val eq'4 : ('a * 'b * 'c * 'd) bpr
