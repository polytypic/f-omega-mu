type 'a t = 'a

include Higher.F'1 with type 'a t'1 = 'a

val run : (f, 'a) Monad.frm -> 'a
