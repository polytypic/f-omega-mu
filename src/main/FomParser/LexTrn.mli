open StdlibPlus
open FomSource

type 't t = 't * Pos.t * Pos.t

include Higher.F'2

type ('t, 'a) mr = 't f'1 Monad.t -> ('t, 'a) f'2

(* *)

val left_of : 't t -> int
val right_of : 't t -> int
val tok_of : 't t -> 't
val set : 't -> 't t -> 't t

(* *)

val get : ('t, 't t) mr
val unget : 't t -> ('t, unit) mr

(* *)

val emit : 't t -> ('t, unit) mr
val emit_if : bool -> 't t -> ('t, unit) mr
val emit_before : 't -> 't t -> ('t, unit) mr

(* *)

val is_typ : ('t, bool) mr
val as_typ : ('t, 'a) mr -> ('t, 'a) mr

(* *)

val loc : ('t, Loc.t) mr
val last_tok : ('t, 't t) mr
val new_line : 't t -> ('t, bool) mr
val with_indent : (int -> 't t -> ('t, 'a) mr) -> ('t, 'a) mr

(* *)

val init : (Buffer.t -> 't t) -> ('t, unit) mr -> Buffer.t -> unit -> 't t
