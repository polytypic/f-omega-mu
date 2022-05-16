open FomBasis
open FomSource
open FomPPrint
open FomError
open FomChecker

type t = Loc.t * document

val of_error :
  [< Error.t] ->
  ( (< 'r Kind.UnkEnv.f
     ; ([> `Kind of Kind.t], 'r) Typ.VarEnv.f
     ; 'r Typ.Solved.f
     ; .. >
     as
     'r),
    'e,
    t * t list )
  rea

val pp : t * t list -> document
