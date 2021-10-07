open FomBasis
open FomSource
open FomAnnot
open FomDiag
open FomChecker

module Path : sig
  val inc_ext : string
  val sig_ext : string
  val mod_ext : string

  (* *)

  val ensure_ext : string -> string -> string

  (* *)

  val is_http : string -> bool
  val coalesce : Loc.t -> JsonString.t -> string
end

module Fetch : sig
  type e = [Error.file_doesnt_exist | Error.io_error]
  type t = Loc.t -> string -> (unit, e, string) rea

  val dummy : t

  class con :
    t
    -> object
         method fetch : t
       end
end

module Parameters : sig
  type t

  class con :
    object ('r)
      method parameters : (t, 'r) Field.t
    end
end

module TypIncludes : sig
  type t

  val create : unit -> t

  class con :
    t
    -> object ('r)
         method typ_includes : t
       end
end

module TypImports : sig
  type t

  val create : unit -> t

  class con :
    t
    -> object ('r)
         method typ_imports : t
       end
end

module ExpImports : sig
  type t

  val create : unit -> t

  val get :
    string ->
    ( < exp_imports : t ; .. >,
      [> Error.t],
      FomAST.Exp.Var.t * FomAST.Exp.t * FomAST.Typ.t * string list )
    rea

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

val elaborate_typ :
  FomCST.Typ.t ->
  ( (< annotations : (Annot.t, 'r) Field.t
     ; fetch : Fetch.t
     ; import_chain : (ImportChain.t, 'r) Field.t
     ; kind_env : (Kind.UnkMap.t, 'r) Field.t
     ; typ_env : ([`Kind of Kind.t | `Typ of Typ.t] Typ.VarMap.t, 'r) Field.t
     ; typ_includes : TypIncludes.t
     ; typ_imports : TypImports.t
     ; parameters : (Parameters.t, 'r) Field.t
     ; .. >
     as
     'r),
    [> Error.t],
    FomAST.Typ.t )
  rea

val elaborate :
  FomCST.Exp.t ->
  ( (< annotations : (Annot.t, 'r) Field.t
     ; fetch : Fetch.t
     ; import_chain : (ImportChain.t, 'r) Field.t
     ; kind_env : (Kind.UnkMap.t, 'r) Field.t
     ; typ_env : ([`Kind of Kind.t | `Typ of Typ.t] Typ.VarMap.t, 'r) Field.t
     ; typ_includes : TypIncludes.t
     ; typ_imports : TypImports.t
     ; exp_env : (Exp.VarMap.t, 'r) Field.t
     ; exp_imports : ExpImports.t
     ; parameters : (Parameters.t, 'r) Field.t
     ; .. >
     as
     'r),
    [> Error.t],
    FomAST.Exp.t * FomAST.Typ.t * string list )
  rea
