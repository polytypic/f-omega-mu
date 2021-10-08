open FomPP

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
