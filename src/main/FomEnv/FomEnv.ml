open FomAnnot

module Env = struct
  let empty ?(annot : Annot.t = Annot.empty ()) () =
    object
      method annotations = annot
      inherit FomChecker.Exp.Env.con
      inherit FomChecker.Typ.Env.con
      inherit FomElab.TypAliases.con
      inherit FomElab.Includes.con
      inherit FomElab.Imports.con
    end
end
