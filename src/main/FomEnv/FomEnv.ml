open FomBasis
open FomAnnot
open FomChecker
open FomElab

module Env = struct
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

  let empty ?(fetch = FomElab.Fetch.dummy)
      ?(typ_includes = FomElab.TypIncludes.create ())
      ?(typ_imports = FomElab.TypImports.create ())
      ?(exp_imports = FomElab.ExpImports.create ())
      ?(annot = FomAnnot.Annot.empty ()) () =
    object
      inherit FomAnnot.Annot.con annot
      inherit FomChecker.Exp.VarMap.con
      inherit [_] FomChecker.Typ.VarMap.con
      inherit FomChecker.Typ.Solved.con
      inherit FomChecker.Kind.UnkMap.con
      inherit FomElab.Parameters.con
      inherit [_] FomElab.Fetch.con fetch
      inherit FomElab.ImportChain.con
      inherit FomElab.TypIncludes.con typ_includes
      inherit FomElab.TypImports.con typ_imports
      inherit FomElab.ExpImports.con exp_imports
    end
end
