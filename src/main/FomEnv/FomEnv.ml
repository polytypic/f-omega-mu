open FomAnnot
open FomChecker
open FomElab
open FomToJsC

module Env = struct
  let empty ?(annot = Annot.empty ()) ?(exp_imports = ExpImports.create ())
      ?(fetch = Fetch.dummy) ?(mod_in_js = ModInJs.create ())
      ?(mod_simplified = ModSimplified.create ())
      ?(typ_imports = TypImports.create ())
      ?(typ_includes = TypIncludes.create ()) () =
    object
      inherit Annot.con annot
      inherit Exp.VarMap.con
      inherit ExpImports.con exp_imports
      inherit [_] Fetch.con fetch
      inherit ImportChain.con
      inherit Kind.UnkMap.con
      inherit ModInJs.con mod_in_js
      inherit ModSimplified.con mod_simplified
      inherit Parameters.con
      inherit Typ.Solved.con
      inherit [_] Typ.VarMap.con
      inherit Typ.UnkMap.con
      inherit TypImports.con typ_imports
      inherit TypIncludes.con typ_includes
    end
end
