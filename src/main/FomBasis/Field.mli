open Misc.Syntax

type ('a, 'r) t

val make : 'a -> ('a -> 'r) -> ('a, 'r) t
val get : ('r -> ('a, 'r) t) -> 'r -> 'a
val map : ('r -> ('a, 'r) t) -> 'a uop -> 'r uop
val set : ('r -> ('a, 'r) t) -> 'a -> 'r uop
