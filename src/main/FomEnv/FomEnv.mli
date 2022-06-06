open FomAnnot
open FomChecker
open FomElab
open FomToJsC

module Env : sig
  val empty :
    ?annot:Annot.t ->
    ?exp_imports:ExpImports.t ->
    ?fetch:'r Fetch.m ->
    ?mod_in_js:ModInJs.t ->
    ?mod_simplified:ModSimplified.t ->
    ?typ_imports:TypImports.t ->
    ?typ_includes:TypIncludes.t ->
    unit ->
    (< 'r Annot.f
     ; 'r Exp.VarEnv.f
     ; 'r ExpImports.f
     ; 'r Fetch.f
     ; 'r ImportChain.f
     ; 'r Kind.UnkEnv.f
     ; 'r ModInJs.f
     ; 'r ModSimplified.f
     ; 'r Parameters.f
     ; 'r Typ.Goals.f
     ; 'r Typ.Solved.f
     ; 'r Typ.UnkEnv.f
     ; 'r TypImports.f
     ; 'r TypIncludes.f
     ; ('t, 'r) Typ.VarEnv.f >
     as
     'r)
end
