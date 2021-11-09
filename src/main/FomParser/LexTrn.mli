open FomBasis
open FomSource

type 't t = 't * Pos.t * Pos.t

include Higher.F'2

type ('t, 'a) fr = ('t, f) app'1 Monad.t -> ('t, 'a, f) app'2

(* *)

val col_of : 't t -> int
val tok_of : 't t -> 't
val set : 't -> 't t -> 't t

(* *)

val get : ('t, 't t) fr
val unget : 't t -> ('t, unit) fr

(* *)

val emit : 't t -> ('t, unit) fr
val emit_if : bool -> 't t -> ('t, unit) fr
val emit_before : 't -> 't t -> ('t, unit) fr

(* *)

val is_typ : ('t, bool) fr
val as_typ : ('t, 'a) fr -> ('t, 'a) fr

(* *)

val loc : ('t, Loc.t) fr
val last_tok : ('t, 't t) fr
val new_line : 't t -> ('t, bool) fr
val with_indent : (int -> 't t -> ('t, 'a) fr) -> ('t, 'a) fr

(* *)

val init : (Buffer.t -> 't t) -> ('t, unit) fr -> Buffer.t -> unit -> 't t
