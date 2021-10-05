module Env = struct
  let empty ?(fetch = FomElab.Fetch.dummy)
      ?(typ_includes = FomElab.TypIncludes.create ())
      ?(typ_imports = FomElab.TypImports.create ())
      ?(exp_imports = FomElab.ExpImports.create ())
      ?(annot = FomAnnot.Annot.empty ()) () =
    object
      inherit FomAnnot.Annot.con annot
      inherit FomChecker.Exp.VarMap.con
      inherit [_] FomChecker.Typ.VarMap.con
      inherit FomChecker.Kind.UnkMap.con
      inherit FomElab.Parameters.con
      inherit FomElab.Fetch.con fetch
      inherit FomElab.ImportChain.con
      inherit FomElab.TypIncludes.con typ_includes
      inherit FomElab.TypImports.con typ_imports
      inherit FomElab.ExpImports.con exp_imports
    end
end
