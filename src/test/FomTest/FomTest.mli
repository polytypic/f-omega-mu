open FomBasis

val test : string -> (unit -> (unit, exn, unit) Rea.t) -> unit
val verify : bool -> (unit, exn, unit) Rea.t
