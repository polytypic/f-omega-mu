open FomBasis
open FomSource
open FomAnnot
open FomDiag

module Path : sig
  val inc_ext : string
  val sig_ext : string
  val mod_ext : string

  (* *)

  val ensure_ext : string -> string -> string

  (* *)

  val is_http : string -> bool
  val resolve : Loc.t -> FomAST.LitString.t -> string
end

module Fetch : sig
  type e = [Error.file_doesnt_exist | Error.io_error]
  type t = Loc.t -> string -> (unit, e, string) Rea.t

  class con :
    t
    -> object
         method fetch : t
       end

  val fetch : Loc.t -> string -> (< fetch : t ; .. >, [> e], string) Rea.t
end

module TypAliases : sig
  type t = FomAST.Typ.t FomAST.Typ.Env.t

  val field : (< typ_aliases : (t, 'r) Field.t ; .. > as 'r) -> (t, 'r) Field.t

  class con :
    object ('r)
      method typ_aliases : (t, 'r) Field.t
    end
end

module TypIncludes : sig
  type e = [Error.io_error | Error.syntax_errors | Error.source_errors]
  type t = (string, (e, FomAST.Typ.t FomAST.Typ.Env.t) IVar.t) Hashtbl.t

  val field : (< typ_includes : t ; .. > as 'r) -> t

  class con :
    t
    -> object ('r)
         method typ_includes : t
       end
end

module TypImports : sig
  type e = [Error.io_error | Error.syntax_errors | Error.source_errors]
  type t = (string, (e, FomAST.Typ.t) IVar.t) Hashtbl.t

  val field : (< typ_imports : t ; .. > as 'r) -> t

  class con :
    t
    -> object ('r)
         method typ_imports : t
       end
end

module ExpImports : sig
  type e = [Error.io_error | Error.syntax_errors | Error.source_errors]

  type t =
    ( string,
      (e, FomAST.Exp.Id.t * FomAST.Exp.t * FomAST.Typ.t option) IVar.t )
    Hashtbl.t

  val field : (< exp_imports : t ; .. > as 'r) -> t

  class con :
    t
    -> object ('r)
         method exp_imports : t
       end
end

module ImportChain : sig
  type t

  class con :
    object ('r)
      method import_chain : (t, 'r) Field.t
    end
end

val elaborate_defs :
  FomCST.Typ.t FomCST.Typ.Def.f list ->
  ( (< annotations : Annot.t
     ; fetch : Fetch.t
     ; import_chain : (ImportChain.t, 'r) Field.t
     ; typ_aliases : (TypAliases.t, 'r) Field.t
     ; typ_includes : TypIncludes.t
     ; typ_imports : TypImports.t
     ; .. >
     as
     'r),
    [> TypImports.e],
    FomAST.Typ.t FomAST.Typ.Env.t )
  Rea.t

val elaborate_typ :
  FomCST.Typ.t ->
  ( (< annotations : Annot.t
     ; fetch : Fetch.t
     ; import_chain : (ImportChain.t, 'r) Field.t
     ; typ_aliases : (TypAliases.t, 'r) Field.t
     ; typ_includes : TypIncludes.t
     ; typ_imports : TypImports.t
     ; .. >
     as
     'r),
    [> Error.io_error | Error.syntax_errors | Error.source_errors],
    FomAST.Typ.t )
  Rea.t

val elaborate :
  FomCST.Exp.t ->
  ( (< annotations : Annot.t
     ; fetch : Fetch.t
     ; typ_aliases : (TypAliases.t, 'r) Field.t
     ; import_chain : (ImportChain.t, 'r) Field.t
     ; typ_includes : TypIncludes.t
     ; typ_imports : TypImports.t
     ; exp_imports : ExpImports.t
     ; .. >
     as
     'r),
    [> ExpImports.e],
    FomAST.Exp.t )
  Rea.t

val with_modules :
  FomAST.Exp.t ->
  ( (< exp_imports : ExpImports.t ; .. > as 'r),
    [> ExpImports.e],
    FomAST.Exp.t )
  Rea.t
