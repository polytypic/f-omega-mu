open FomSource
open FomPP
open FomError

type t = Loc.t * document

val of_error : [< Error.t] -> t * t list
val pp : t * t list -> document
