type 'a t = 'a * 'a

val exists : ('a -> bool) -> 'a t -> bool

val map_fr :
  ('a -> ('f, 'F, 'b) Applicative.fr) -> 'a t -> ('f, 'F, 'b t) Applicative.fr

val map_eq_fr :
  ('a -> ('f, 'F, 'a) Applicative.fr) -> 'a t -> ('f, 'F, 'a t) Applicative.fr
