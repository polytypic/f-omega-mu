include module type of Stdlib.String
open Misc.Syntax

val is_prefix : string bpr
val is_suffix : string bpr
val drop : int -> string uop
val drop_last : int -> string uop
val split : int -> string -> string * string
val split_on_char : char -> string -> string list
val filter : (char -> bool) -> string uop
