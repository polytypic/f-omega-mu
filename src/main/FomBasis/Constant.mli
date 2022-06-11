open Higher.Syntax

type ('c, 'a) t

include Higher.F'2 with type ('c, 'a) t'2 = ('c, 'a) t

type 'c f'1 = ('c, f) app'1
type ('c, 'a) f'2 = ('c, 'a, f) app'2

val ( let+ ) : ('a, 'b, 'c f'1) Functor.map

(* *)

val inject : 'c -> ('c, 'a) f'2
val project : ('c, 'a) f'2 -> 'c

(* *)

val m : 'a f'1 Functor.t
val from : 'c -> 'm -> ('c, 'a) f'2
val run : 'm -> ('m -> ('c, 'a) f'2) -> 'c

(* *)

val of_monoid : < 'c Monoid.t ; .. > -> 'c f'1 Applicative.t

(* *)

val cat_m : 'a Cat.t f'1 Applicative.t
val or_lm : bool Lazy.t f'1 Applicative.t
val option_lm : 'a Option.t Lazy.t f'1 Applicative.t
val option_m : 'a Option.t f'1 Applicative.t
val unit_fr_m : ('f, 'F, unit) Monad.fr f'1 Applicative.t
