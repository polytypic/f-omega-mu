open FomBasis
open FomAnnot

module TypAliases : sig
  type t = FomAST.Typ.t FomAST.Typ.Env.t

  val field : (< typ_aliases : (t, 'r) Field.t ; .. > as 'r) -> (t, 'r) Field.t

  class con :
    object ('r)
      method typ_aliases : (t, 'r) Field.t
    end
end

module TypIncludes : sig
  type t = FomAST.Typ.t FomAST.Typ.Env.t FomCST.Typ.IncludeMap.t

  val field : (< typ_includes : (t, 'r) Field.t ; .. > as 'r) -> (t, 'r) Field.t

  class con :
    object ('r)
      method typ_includes : (t, 'r) Field.t
    end
end

module TypImports : sig
  type t = FomAST.Typ.t FomCST.Typ.ImportMap.t

  val field : (< typ_imports : (t, 'r) Field.t ; .. > as 'r) -> (t, 'r) Field.t

  class con :
    object ('r)
      method typ_imports : (t, 'r) Field.t
    end
end

module ExpImports : sig
  type t = FomAST.Exp.Id.t FomCST.Exp.ImportMap.t

  val field : (< exp_imports : (t, 'r) Field.t ; .. > as 'r) -> (t, 'r) Field.t

  class con :
    object ('r)
      method exp_imports : (t, 'r) Field.t
    end
end

val elaborate_defs :
  FomCST.Typ.t FomCST.Typ.Def.f list ->
  ( (< annotations : Annot.t
     ; typ_aliases : (TypAliases.t, 'r) Field.t
     ; typ_includes : (TypIncludes.t, 'r) Field.t
     ; typ_imports : (TypImports.t, 'r) Field.t
     ; .. >
     as
     'r),
    'x,
    FomAST.Typ.t FomAST.Typ.Env.t )
  Reader.t

val elaborate_typ :
  FomCST.Typ.t ->
  ( (< annotations : Annot.t
     ; typ_aliases : (TypAliases.t, 'r) Field.t
     ; typ_includes : (TypIncludes.t, 'r) Field.t
     ; typ_imports : (TypImports.t, 'r) Field.t
     ; .. >
     as
     'r),
    'x,
    FomAST.Typ.t )
  Reader.t

val elaborate :
  FomCST.Exp.t ->
  ( (< annotations : Annot.t
     ; typ_aliases : (TypAliases.t, 'r) Field.t
     ; typ_includes : (TypIncludes.t, 'r) Field.t
     ; typ_imports : (TypImports.t, 'r) Field.t
     ; exp_imports : (ExpImports.t, 'r) Field.t
     ; .. >
     as
     'r),
    'x,
    FomAST.Exp.t )
  Reader.t
