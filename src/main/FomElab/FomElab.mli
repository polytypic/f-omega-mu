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

module Includes : sig
  type t = FomAST.Typ.t FomAST.Typ.Env.t FomCST.Typ.IncludeMap.t

  val field : (< includes : (t, 'r) Field.t ; .. > as 'r) -> (t, 'r) Field.t

  class con :
    object ('r)
      method includes : (t, 'r) Field.t
    end
end

module Imports : sig
  type t = FomAST.Exp.Id.t FomCST.Exp.ImportMap.t

  val field : (< imports : (t, 'r) Field.t ; .. > as 'r) -> (t, 'r) Field.t

  class con :
    object ('r)
      method imports : (t, 'r) Field.t
    end
end

val elaborate_defs :
  FomCST.Typ.t FomCST.Typ.Def.f list ->
  ( 'x,
    (< annotations : Annot.t
     ; typ_aliases : (TypAliases.t, 'r) Field.t
     ; includes : (Includes.t, 'r) Field.t
     ; .. >
     as
     'r),
    FomAST.Typ.t FomAST.Typ.Env.t )
  Reader.t

val elaborate_typ :
  FomCST.Typ.t ->
  ( 'x,
    (< annotations : Annot.t
     ; typ_aliases : (TypAliases.t, 'r) Field.t
     ; includes : (Includes.t, 'r) Field.t
     ; .. >
     as
     'r),
    FomAST.Typ.t )
  Reader.t

val elaborate :
  FomCST.Exp.t ->
  ( 'x,
    (< annotations : Annot.t
     ; typ_aliases : (TypAliases.t, 'r) Field.t
     ; includes : (Includes.t, 'r) Field.t
     ; imports : (Imports.t, 'r) Field.t
     ; .. >
     as
     'r),
    FomAST.Exp.t )
  Reader.t
