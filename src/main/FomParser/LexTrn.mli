open Rea
open StdlibPlus
open FomSource

type 't t = 't * Pos.t * Pos.t
type ('t, 'a) m
type 't r

val to_rea : ('t, 'a) m -> ('t r, 'e, 'a) s
val of_rea : ('t r, 'e, 'a) s -> ('t, 'a) m

(* *)

val left_of : 't t -> int
val right_of : 't t -> int
val tok_of : 't t -> 't
val set : 't -> 't t -> 't t

(* *)

val get : ('t r, 't, 't t, (('t r, 'D) #monad' as 'D)) er
val unget : 't t -> ('t r, 't, unit, (('t r, 'D) #monad' as 'D)) er

(* *)

val emit : 't t -> ('t r, 't, unit, (('t r, 'D) #monad' as 'D)) er
val emit_if : bool -> 't t -> ('t r, 't, unit, (('t r, 'D) #monad' as 'D)) er
val emit_before : 't -> 't t -> ('t r, 't, unit, (('t r, 'D) #monad' as 'D)) er

(* *)

val is_typ : ('t r, 't, bool, (('t r, 'D) #monad' as 'D)) er

val as_typ :
  ('t r, 't, 'a, (('t r, 'D) #monad' as 'D)) er ->
  ('t r, 't, 'a, (('t r, 'D) #monad' as 'D)) er

(* *)

val loc : ('t r, 't, Loc.t, (('t r, 'D) #monad' as 'D)) er
val last_tok : ('t r, 't, 't t, (('t r, 'D) #monad' as 'D)) er
val new_line : 't t -> ('t r, 't, bool, (('t r, 'D) #monad' as 'D)) er

val with_indent :
  (int -> 't t -> ('t r, 't, 'a, (('t r, 'D) #monad' as 'D)) er) ->
  ('t r, 't, 'a, (('t r, 'D) #monad' as 'D)) er

(* *)

val init :
  (Buffer.t -> 't t) ->
  ('t r, 't, unit, (('t r, 'D) monad' as 'D)) er ->
  Buffer.t ->
  unit ->
  't t
