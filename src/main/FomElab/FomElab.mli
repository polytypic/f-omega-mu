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
  type 'r t = Loc.t -> string -> ('r, e, string) rea

  val dummy : 'r t

  class ['r] con :
    'r t
    -> object
         method fetch : 'r t
       end

  type 'r f = 'r con
end

module Parameters : sig
  type t

  class con :
    object ('r)
      method parameters : (t, 'r) Field.t
    end

  type 'r f = < parameters : (t, 'r) Field.t >
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
  type t

  class con :
    object ('r)
      method import_chain : (t, 'r) Field.t
    end

  type 'r f = < import_chain : (t, 'r) Field.t >
end

val elaborate_typ :
  FomCST.Typ.t ->
  ( (< 'r Annot.f
     ; 'r Fetch.f
     ; 'r ImportChain.f
     ; 'r Kind.UnkMap.f
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
     ; 'r Kind.UnkMap.f
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
