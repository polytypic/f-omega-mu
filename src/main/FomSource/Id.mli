open StdlibPlus
open FomPPrint

module Counter : sig
  type t

  val compare : t cmp
end

module Name : sig
  type t

  val compare : t cmp

  (* *)

  val underscore : t
  val fresh : t
end

module type S = sig
  type t = {name : Name.t; n : Counter.t; at : Loc.t}

  val at : t -> Loc.t
  val set_at : Loc.t -> t -> t
  val name : t -> Name.t

  (* Special *)

  val is_numeric : t -> bool
  val is_fresh : t -> bool
  val is_underscore : t -> bool

  (* Comparison *)

  val equal : t -> t -> bool
  val compare : t -> t -> int

  (* Formatting *)

  val to_string : t -> string
  val pp : ?hr:bool -> t -> document

  (* Constructors *)

  val underscore : Loc.t -> t
  val of_string : Loc.t -> string -> t
  val of_name : Loc.t -> Name.t -> t
  val of_number : Loc.t -> Bigint.t -> t

  (* Generated *)

  val fresh : Loc.t -> t

  (* Freshening *)

  val freshen : t -> t

  module Unsafe : sig
    val set_counter : int -> t -> t
    val smallest : (t -> ('f, 'F, bool) Monad.fr) -> t -> ('f, 'F, t) Monad.fr
  end
end

module Make () : S
