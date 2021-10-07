open FomBasis

val test : string -> (unit -> (unit, exn, unit) rea) -> unit
val verify : bool -> (unit, exn, unit) rea
