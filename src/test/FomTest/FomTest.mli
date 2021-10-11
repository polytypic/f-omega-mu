open FomBasis

val test : string -> (unit -> (unit, exn, unit) rea) -> unit
val verify : bool -> (unit, exn, unit) rea
val failure : string -> ('r, exn, 'a) rea

val failuref :
  ('a, unit, string, string, string, ('r, exn, 'b) rea) format6 -> 'a
