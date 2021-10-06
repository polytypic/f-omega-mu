open FomPP

module Pos : sig
  type t = Lexing.position

  (* Constructors *)

  val of_path : string -> t

  (* Accessors *)

  val column_of : t -> int

  (* Comparison *)

  val compare : t -> t -> int
end

module Loc : sig
  type t = Pos.t * Pos.t

  (* Comparison *)

  val compare : t -> t -> int

  (* Constructors *)

  val of_path : string -> t
  val dummy : t
  val union : t -> t -> t

  (* Accessors *)

  val path : t -> string

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

    (* Generated *)

    val fresh : Loc.t -> t

    (* Freshening *)

    val freshen : t -> t
  end

  module Make () : S
end
