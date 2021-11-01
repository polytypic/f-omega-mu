include Map.OrderedType

val of_utf8_json : string -> t
val to_utf8_json : t -> string

(* *)

val of_utf8 : string -> t
val to_utf8 : t -> string

(* *)

val of_utf8_json_literal : string -> t

(* *)

val empty : t
val is_empty : t -> bool
