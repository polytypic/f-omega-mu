include module type of Stdlib.Array

val choose : ('a -> 'b option) -> 'a t -> 'b t
val sorted : ('a -> 'a -> int) -> 'a t -> 'a t
