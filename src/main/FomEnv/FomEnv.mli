open FomAnnot
open FomChecker
open FomElab
open FomToJsC

module Env : sig
  val empty :
    ?annot:Annot.t ->
    ?exp_imports:ExpImports.t ->
    ?fetch:Fetch.t ->
    ?mod_in_js:ModInJs.t ->
    ?mod_simplified:ModSimplified.t ->
    ?typ_imports:TypImports.t ->
    ?typ_includes:TypIncludes.t ->
    unit ->
    < Annot.con
    ; Exp.VarEnv.con
    ; ExpImports.con
    ; Fetch.con
    ; ImportChain.con
    ; Kind.UnkEnv.con
    ; ModInJs.con
    ; ModSimplified.con
    ; Parameters.con
    ; Typ.Goals.con
    ; Typ.Solved.con
    ; TypImports.con
    ; TypIncludes.con
    ; 't Typ.VarEnv.con >
end
