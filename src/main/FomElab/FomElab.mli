open FomBasis
open FomAnnot

val elaborate_defs :
  FomCST.Typ.t FomCST.Typ.Def.f list ->
  (< annotations : Annot.t
   ; get_typ_aliases : FomAST.Typ.t FomAST.Typ.Env.t
   ; map_typ_aliases : FomAST.Typ.t FomAST.Typ.Env.t uop -> 'r
   ; get_includes : FomAST.Typ.t FomAST.Typ.Env.t FomCST.Typ.IncludeMap.t
   ; .. >
   as
   'r) ->
  FomAST.Typ.t FomAST.Typ.Env.t

val elaborate_typ :
  FomCST.Typ.t ->
  (< annotations : Annot.t
   ; get_typ_aliases : FomAST.Typ.t FomAST.Typ.Env.t
   ; map_typ_aliases : FomAST.Typ.t FomAST.Typ.Env.t uop -> 'r
   ; get_includes : FomAST.Typ.t FomAST.Typ.Env.t FomCST.Typ.IncludeMap.t
   ; .. >
   as
   'r) ->
  FomAST.Typ.t

val elaborate :
  FomCST.Exp.t ->
  (< annotations : Annot.t
   ; get_typ_aliases : FomAST.Typ.t FomAST.Typ.Env.t
   ; map_typ_aliases : FomAST.Typ.t FomAST.Typ.Env.t uop -> 'r
   ; get_includes : FomAST.Typ.t FomAST.Typ.Env.t FomCST.Typ.IncludeMap.t
   ; get_imports : FomAST.Exp.Id.t FomCST.Exp.ImportMap.t
   ; .. >
   as
   'r) ->
  FomAST.Exp.t
