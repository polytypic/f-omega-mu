type ('e, 'a) t = [`Ok of 'a | `Error of 'e]

val catch : (unit -> 'a) -> (exn, 'a) t
