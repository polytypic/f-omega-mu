open Misc.Syntax
include module type of Stdlib.List

val for_alli : (int -> 'a -> bool) -> 'a list -> bool
val equal_with : 'a bpr -> 'a list bpr
val compare_with : 'a cmp -> 'a list cmp
val find_dup_opt : 'a cmp -> 'a list -> ('a * 'a) option

(* *)

val map_phys_eq : 'a uop -> 'a list uop
val share_phys_eq : 'a bop -> 'a list bop

(* *)

val fold_left_fr :
  ('a -> 'b -> ('f, 'F, 'a) Monad.fr) -> 'a -> 'b t -> ('f, 'F, 'a) Monad.fr

val fold_left2_fr :
  ('a -> 'b -> 'c -> ('f, 'F, 'a) Monad.fr) ->
  'a ->
  'b t ->
  'c t ->
  ('f, 'F, 'a) Monad.fr

(* *)

val iter_fr : ('a -> ('f, 'F, unit) Monad.fr) -> 'a t -> ('f, 'F, unit) Monad.fr

val iter2_fr :
  ('a -> 'b -> ('f, 'F, unit) Monad.fr) ->
  'a t ->
  'b t ->
  ('f, 'F, unit) Monad.fr

(* *)

val for_all_fr :
  ('a -> ('f, 'F, bool) Monad.fr) -> 'a t -> ('f, 'F, bool) Monad.fr

(* *)

val find_opt_fr :
  ('a -> ('f, 'F, bool) Monad.fr) -> 'a t -> ('f, 'F, 'a option) Monad.fr

(* *)

val map_fr :
  ('a -> ('f, 'F, 'b) Applicative.fr) -> 'a t -> ('f, 'F, 'b t) Applicative.fr

val map_phys_eq_fr :
  ('a -> ('f, 'F, 'a) Applicative.fr) -> 'a t -> ('f, 'F, 'a t) Applicative.fr
