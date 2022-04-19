open Higher.Syntax

type ('c, 'a) t

include Higher.F'2 with type ('c, 'a) t'2 = ('c, 'a) t

val ( let+ ) : ('a, 'b, ('c, f) app'1) Functor.map

(* *)

val from : 'c -> ('c, 'a, f) app'2
val eval : ('c, 'a, f) app'2 -> 'c

(* *)

val inj'0 : 'c -> (('c, f) app'1, 'F, 'a) Functor.fr
val inj'1 : ('x -> 'c) -> 'x -> (('c, f) app'1, 'F, 'a) Functor.fr
val run : (('c, f) app'1 Functor.t -> ('c, 'a, f) app'2) -> 'c

(* *)

val of_monoid : < 'c Monoid.t ; .. > -> ('c, f) app'1 Applicative.t

(* *)

val or_lm : (bool Lazy.t, f) app'1 Applicative.t
val option_lm : ('a Option.t Lazy.t, f) app'1 Applicative.t
