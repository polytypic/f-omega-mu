include module type of Stdlib.Seq

val exists_fr :
  ('a -> ('f, 'F, bool) Monad.fr) -> 'a t -> ('f, 'F, bool) Monad.fr
