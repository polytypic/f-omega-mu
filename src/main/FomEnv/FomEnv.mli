open FomBasis
open FomAnnot
open FomChecker
open FomElab

module Env : sig
  type 't t =
    < annotations : (Annot.t, 'r) Field.t
    ; exp_env : (Exp.VarMap.t, 'r) Field.t
    ; exp_imports : ExpImports.t
    ; fetch : 'r Fetch.t
    ; import_chain : (ImportChain.t, 'r) Field.t
    ; kind_env : (Kind.UnkMap.t, 'r) Field.t
    ; parameters : (Parameters.t, 'r) Field.t
    ; typ_env : ('t Typ.VarMap.t, 'r) Field.t
    ; typ_imports : TypImports.t
    ; typ_includes : TypIncludes.t
    ; typ_solved : (Typ.Solved.t, 'r) Field.t >
    as
    'r

  val empty :
    ?fetch:'t t Fetch.t ->
    ?typ_includes:TypIncludes.t ->
    ?typ_imports:TypImports.t ->
    ?exp_imports:ExpImports.t ->
    ?annot:Annot.t ->
    unit ->
    't t
end
