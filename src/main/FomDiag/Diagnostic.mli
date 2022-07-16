open StdlibPlus
open FomSource
open FomPPrint
open FomError
open FomChecker

type t = Loc.t * document

val of_error :
  [< Error.t] ->
  ( < Kind.UnkEnv.con
    ; [> `Kind of Kind.t] Typ.VarEnv.con
    ; Typ.Goals.con
    ; Typ.Solved.con
    ; .. >,
    'e,
    t * t list )
  rea

val pp : t * t list -> document
