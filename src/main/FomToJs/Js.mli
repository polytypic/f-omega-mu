open FomBasis

val is_identity : JsonString.t -> bool

val is_illegal_id : string -> bool
(** Tests whether given name should be considered illegal identifier in JS. *)

val is_safe_nat : string -> bool
