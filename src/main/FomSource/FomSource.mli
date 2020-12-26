open FomPP

module Pos : sig
  type t = Lexing.position

  val column_of : t -> int
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
  module type S = sig
    type t = {it : string; at : Loc.t}

    (* Comparison *)

    val equal : t -> t -> bool
    val compare : t -> t -> int

    (* Formatting *)

    val pp : t -> document

    (* Constructors *)

    val id : Loc.t -> string -> t

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
