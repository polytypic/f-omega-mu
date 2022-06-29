open Misc.Syntax

val set_fst : 'a * 'b -> 'c -> 'c * 'b
val set_snd : 'a * 'b -> 'c -> 'a * 'c

val swap : 'a * 'b -> 'b * 'a
(** Swap elements of a pair. *)

val map : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
val map_phys_eq : 'a uop -> 'b uop -> ('a * 'b) uop
val share_phys_eq : 'a bop -> 'b bop -> ('a * 'b) bop

(* *)

val map_fr :
  ('a -> ('f, 'F, 'b) Applicative.fr) ->
  ('c -> ('f, 'F, 'd) Applicative.fr) ->
  'a * 'c ->
  ('f, 'F, 'b * 'd) Applicative.fr

val map_phys_eq_fr :
  ('a -> ('f, 'F, 'a) Applicative.fr) ->
  ('b -> ('f, 'F, 'b) Applicative.fr) ->
  'a * 'b ->
  ('f, 'F, 'a * 'b) Applicative.fr
