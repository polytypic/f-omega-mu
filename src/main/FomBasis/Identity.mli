open Higher.Syntax

type 'a t = 'a

include Higher.F'1 with type 'a t'1 = 'a

type 'a fr = f Monad.t -> ('a, f) app'1

val inj'1 : ('a -> 'b) -> 'a -> 'b fr
val run : 'a fr -> 'a
