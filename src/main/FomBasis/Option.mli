open Higher.Syntax
include module type of Stdlib.Option

val map_fr :
  ('a -> ('f, 'F, 'b) Applicative.fr) -> 'a t -> ('f, 'F, 'b t) Applicative.fr

val map_eq_fr :
  ('a -> ('f, 'F, 'a) Applicative.fr) -> 'a t -> ('f, 'F, 'a t) Applicative.fr

val iter_fr :
  ('a -> ('f, 'F, unit) Applicative.fr) -> 'a t -> ('f, 'F, unit) Applicative.fr

(* *)

val exists : ('a -> bool) -> 'a t -> bool
val or_else : (unit -> 'a t) -> 'a t -> 'a t
val both : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

(* *)

include Higher.F'1 with type 'a t'1 = 'a t

type 'a fr = < f Monad.t ; f Alternative.t > -> ('a, f) app'1

val run : 'a fr -> 'a t
