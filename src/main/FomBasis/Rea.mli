open Higher.Syntax
open Misc.Syntax

type (-'r, +'e, +'a) t

include Higher.F'3 with type ('r, 'e, 'a) t'3 = ('r, 'e, 'a) t

type ('r, 'e, 'a) fr = ('r, 'e, f) app'2 Monad.t -> ('r, 'e, 'a, f) app'3

module Syntax : sig
  type ('r, 'e, 'a) rea = ('r, 'e, 'a) fr

  val start : 'r -> ('r, Zero.t, unit) fr -> unit

  (* *)

  val of_async : (('e -> unit) -> ('a -> unit) -> unit) -> ('r, 'e, 'a) rea
  val of_res : ('e, 'a) Res.t -> ('r, 'e, 'a) rea

  (* *)

  val fail : 'e -> ('r, 'e, 'a) rea

  (* *)

  val try_in :
    ('a -> ('r, 'f, 'b) rea) ->
    ('e -> ('r, 'f, 'b) rea) ->
    ('r, 'e, 'a) rea ->
    ('r, 'f, 'b) rea

  val catch : ('r, 'e, 'a) rea -> ('r, 'f, ('e, 'a) Res.t) rea

  (* *)

  val map_error : ('e -> 'f) -> ('r, 'e, 'a) rea -> ('r, 'f, 'a) rea
  val generalize_error : ('r, Zero.t, 'a) rea -> ('r, 'e, 'a) rea

  (* *)

  val env_as : ('r -> 'a) -> ('r, 'e, 'a) rea
  val with_env : ('r -> 's) -> ('s, 'e, 'a) rea -> ('r, 'e, 'a) rea
  val replace_env : 's -> ('s, 'e, 'a) rea -> ('r, 'e, 'a) rea

  (* *)

  val invoke : ('r -> ('r, 'e, 'a) rea) -> ('r, 'e, 'a) rea

  (* *)

  val get : ('r -> ('f, 'r) Field.t) -> ('r, 'e, 'f) rea
  val get_as : ('r -> ('f, 'r) Field.t) -> ('f -> 'g) -> ('r, 'e, 'g) rea
  val setting : ('r -> ('f, 'r) Field.t) -> 'f -> ('r, 'e, 'a) rea uop
  val mapping : ('r -> ('f, 'r) Field.t) -> 'f uop -> ('r, 'e, 'a) rea uop

  module LVar : sig
    type ('e, 'a) t

    val create : ('r, 'e, 'a) rea -> ('r, 'f, ('e, 'a) t) rea
    val eval : ('e, 'a) t -> ('r, 'e, 'a) rea
  end

  module MVar : sig
    type 'v t

    val create : 'v -> 'v t
    val read : 'v t -> ('r, 'e, 'v) rea
    val mutate : ('v -> 'v) -> 'v t -> ('r, 'e, unit) rea
    val modify : ('v -> 'v * 'a) -> 'v t -> ('r, 'e, 'a) rea
    val try_mutate : ('v -> ('r, 'e, 'v) rea) -> 'v t -> ('r, 'e, unit) rea
    val try_modify : ('v -> ('r, 'e, 'v * 'a) rea) -> 'v t -> ('r, 'e, 'a) rea
  end

  val read : ('r -> ('v MVar.t, 'r) Field.t) -> ('r, 'e, 'v) rea

  val mutate :
    ('r -> ('v MVar.t, 'r) Field.t) -> ('v -> 'v) -> ('r, 'e, unit) rea

  val modify :
    ('r -> ('v MVar.t, 'r) Field.t) -> ('v -> 'v * 'a) -> ('r, 'e, 'a) rea

  val try_mutate :
    ('r -> ('v MVar.t, 'r) Field.t) ->
    ('v -> ('r, 'e, 'v) rea) ->
    ('r, 'e, unit) rea

  val try_modify :
    ('r -> ('v MVar.t, 'r) Field.t) ->
    ('v -> ('r, 'e, 'v * 'a) rea) ->
    ('r, 'e, 'a) rea
end
