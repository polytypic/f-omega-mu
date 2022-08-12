open Rea
open FomSource
open FomPPrint
open FomError
open FomChecker

type t = Loc.t * document

val of_error :
  [< Error.t] ->
  ( 'R,
    'e,
    t * t list,
    (< ('R, 'D) async'
     ; Kind.UnkEnv.con
     ; Typ.Goals.con
     ; Typ.Solved.con
     ; [> `Kind of Kind.t] Typ.VarEnv.con
     ; .. >
     as
     'D) )
  er

val pp : t * t list -> document
