open FomAnnot
open FomChecker
open FomElab
open FomToJsC

module Env : sig
  class ['R, 'D, 't] empty :
    ?annot:Annot.t
    -> ?exp_imports:'R ExpImports.t
    -> ?mod_in_js:'R ModInJs.t
    -> ?mod_simplified:'R ModSimplified.t
    -> ?typ_imports:'R TypImports.t
    -> ?typ_includes:'R TypIncludes.t
    -> unit
    -> object
         inherit Annot.con
         inherit Exp.VarEnv.con
         inherit ['R] ExpImports.con
         inherit ['R, 'D] Fetch.con
         inherit ImportChain.con
         inherit Kind.UnkEnv.con
         inherit ['R] ModInJs.con
         inherit ['R] ModSimplified.con
         inherit Parameters.con
         inherit Typ.Goals.con
         inherit Typ.Solved.con
         inherit ['R] TypImports.con
         inherit ['R] TypIncludes.con
         inherit ['t] Typ.VarEnv.con
       end
end
