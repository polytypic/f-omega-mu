open FomAnnot

val elaborate_typ :
  FomCST.Typ.t ->
  (< annotations : Annot.t
   ; get_typ_aliases : FomAST.Typ.t FomAST.Typ.Env.t
   ; map_typ_aliases :
       (FomAST.Typ.t FomAST.Typ.Env.t -> FomAST.Typ.t FomAST.Typ.Env.t) -> 'r
   ; .. >
   as
   'r) ->
  FomAST.Typ.t

val elaborate :
  FomCST.Exp.t ->
  (< annotations : Annot.t
   ; get_typ_aliases : FomAST.Typ.t FomAST.Typ.Env.t
   ; map_typ_aliases :
       (FomAST.Typ.t FomAST.Typ.Env.t -> FomAST.Typ.t FomAST.Typ.Env.t) -> 'r
   ; .. >
   as
   'r) ->
  FomAST.Exp.t
