open FomBasis
open FomSource
open FomAnnot
open FomError
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
  type 'r m = Loc.t -> string -> ('r, e, string) rea

  val dummy : 'r m

  class con :
    'r m
    -> object ('r)
         method fetch : 'r m
       end

  type 'r f = < fetch : 'r m >
end

module Parameters : sig
  type 'r m

  class con :
    object ('r)
      method parameters : 'r m
    end

  type 'r f = < parameters : 'r m >
end

module TypIncludes : sig
  type t

  val create : unit -> t

  class con :
    t
    -> object
         method typ_includes : t
       end

  type 'r f = con
end

module TypImports : sig
  type t

  val create : unit -> t

  class con :
    t
    -> object
         method typ_imports : t
       end

  type 'r f = con
end

module ExpImports : sig
  type t

  val create : unit -> t

  val get :
    string ->
    ( < exp_imports : t ; .. >,
      [> Error.t],
      (Exp.Var.t * Exp.Core.t * Typ.Core.t * string list) * Annot.map )
    rea

  class con :
    t
    -> object
         method exp_imports : t
       end

  type 'r f = con
end

module ImportChain : sig
  type 'r m

  class con :
    object ('r)
      method import_chain : 'r m
    end

  type 'r f = < import_chain : 'r m >
end

val elaborate_typ :
  FomCST.Typ.t ->
  ( (< 'r Annot.f
     ; 'r Fetch.f
     ; 'r ImportChain.f
     ; 'r Kind.UnkEnv.f
     ; 'r Parameters.f
     ; ([`Kind of Kind.t | `Typ of Typ.t], 'r) Typ.VarMap.f
     ; 'r TypImports.f
     ; 'r TypIncludes.f
     ; 'r Typ.Solved.f
     ; .. >
     as
     'r),
    [> Error.t],
    Typ.t )
  rea

val elaborate :
  FomCST.Exp.t ->
  ( (< 'r Annot.f
     ; 'r Exp.VarMap.f
     ; 'r ExpImports.f
     ; 'r Fetch.f
     ; 'r ImportChain.f
     ; 'r Kind.UnkEnv.f
     ; 'r Parameters.f
     ; ([`Kind of Kind.t | `Typ of Typ.t], 'r) Typ.VarMap.f
     ; 'r TypImports.f
     ; 'r TypIncludes.f
     ; 'r Typ.Solved.f
     ; .. >
     as
     'r),
    [> Error.t],
    Exp.Core.t * Typ.Core.t * string list )
  rea
