open FomBasis
open FomSource
open FomPP
open FomError
open FomChecker

type t = Loc.t * document

val of_error :
  [< Error.t] ->
  ( (< kind_env : (Kind.UnkMap.t, 'r) Field.t
     ; typ_env : ([> `Kind of Kind.t] Typ.VarMap.t, 'r) Field.t
     ; .. >
     as
     'r),
    'e,
    t * t list )
  rea

val pp : t * t list -> document
