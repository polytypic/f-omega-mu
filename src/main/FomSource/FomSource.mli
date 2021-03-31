open FomPP

module Pos : sig
  type t = Lexing.position

  val column_of : t -> int

  (* Comparison *)

  val compare : t -> t -> int
end

module Loc : sig
  type t = Pos.t * Pos.t

  (* Constructors *)

  val dummy : t
  val union : t -> t -> t

  (* Formatting *)

  val pp : t -> document
end

module Id : sig
  module Counter : sig
    type t
  end

  module Name : sig
    type t
  end

  module type S = sig
    type t = {name : Name.t; n : Counter.t; at : Loc.t}

    val at : t -> Loc.t
    val name : t -> Name.t

    (* Comparison *)

    val equal : t -> t -> bool
    val compare : t -> t -> int

    (* Formatting *)

    val to_string : t -> string
    val pp : t -> document

    (* Constructors *)

    val of_string : Loc.t -> string -> t
    val of_name : Loc.t -> Name.t -> t

    (* Generated *)

    val fresh : Loc.t -> t
    val is_fresh : t -> bool

    (* Freshening *)

    val freshen : t -> t
  end

  module Make () : S
end

module Diagnostic : sig
  type diagnostic = Loc.t * document

  exception Error of diagnostic * diagnostic list

  val error : diagnostic -> diagnostic list -> 'a
end
