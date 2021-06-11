open FomBasis
open FomSource
open FomAnnot

module Env = struct
  let empty
      ?(fetch : FomElab.Fetch.t =
        fun _ _ -> Rea.fail @@ `Error_io (Loc.dummy, Failure "fetch"))
      ?(typ_includes : FomElab.TypIncludes.t = Hashtbl.create 100)
      ?(typ_imports : FomElab.TypImports.t = Hashtbl.create 100)
      ?(exp_imports : FomElab.ExpImports.t = Hashtbl.create 100)
      ?(annot : Annot.t = Annot.empty ()) () =
    object
      inherit FomAnnot.Annot.con annot
      inherit FomChecker.Exp.Env.con
      inherit FomChecker.Typ.Env.con
      inherit FomElab.TypAliases.con
      inherit FomElab.Fetch.con fetch
      inherit FomElab.ImportChain.con
      inherit FomElab.TypIncludes.con typ_includes
      inherit FomElab.TypImports.con typ_imports
      inherit FomElab.ExpImports.con exp_imports
    end
end
