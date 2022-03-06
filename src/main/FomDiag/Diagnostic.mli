open FomBasis
open FomSource
open FomPPrint
open FomError
open FomChecker

type t = Loc.t * document

val of_error :
  [< Error.t] ->
  ( (< 'r Kind.UnkMap.f
     ; ([> `Kind of Kind.t], 'r) Typ.VarMap.f
     ; 'r Typ.UnkMap.f
     ; 'r Typ.Solved.f
     ; .. >
     as
     'r),
    'e,
    t * t list )
  rea

val pp : t * t list -> document
