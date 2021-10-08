include module type of Stdlib.Option

val iter_fr :
  ('a -> ('f, 'F, unit) Applicative.fr) -> 'a t -> ('f, 'F, unit) Applicative.fr
