open FomBasis
open FomSource
open FomPP
open FomError
open FomChecker

type t = Loc.t * document

val of_error :
  [< Error.t] ->
  ( (< 'r Kind.UnkMap.f ; ([> `Kind of Kind.t], 'r) Typ.VarMap.f ; .. > as 'r),
    'e,
    t * t list )
  rea

val pp : t * t list -> document
