type t = Lexing.position

(* Constructors *)

val of_path : string -> t

(* Accessors *)

val column_of : t -> int

(* Comparison *)

val compare : t -> t -> int
val equal : t -> t -> bool
