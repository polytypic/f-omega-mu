include module type of Stdlib.Option

val iter_fr :
  ('a -> ('f, 'F, unit) Applicative.fr) -> 'a t -> ('f, 'F, unit) Applicative.fr

val or_else : (unit -> 'a t) -> 'a t -> 'a t
val both : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
