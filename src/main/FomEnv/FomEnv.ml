open FomAnnot
open FomChecker
open FomElab
open FomToJsC

module Env = struct
  class ['R, 'D, 't] empty ?(annot = Annot.empty ())
    ?(exp_imports = ExpImports.create ()) ?(mod_in_js = ModInJs.create ())
    ?(mod_simplified = ModSimplified.create ())
    ?(typ_imports = TypImports.create ())
    ?(typ_includes = TypIncludes.create ()) () =
    object
      inherit Annot.con annot
      inherit Exp.VarEnv.con
      inherit ['R] ExpImports.con exp_imports
      inherit ['R, 'D] Fetch.con
      inherit ImportChain.con
      inherit Kind.UnkEnv.con
      inherit ['R] ModInJs.con mod_in_js
      inherit ['R] ModSimplified.con mod_simplified
      inherit Parameters.con
      inherit Typ.Goals.con
      inherit Typ.Solved.con
      inherit ['t] Typ.VarEnv.con
      inherit ['R] TypImports.con typ_imports
      inherit ['R] TypIncludes.con typ_includes
    end
end
