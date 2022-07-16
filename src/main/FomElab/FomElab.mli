open StdlibPlus
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
  type t = Loc.t -> string -> (unit, e, string) rea

  val dummy : t

  class con :
    t
    -> object
         method fetch : t
       end
end

module Parameters : sig
  type m

  class con :
    object
      method parameters : m
    end
end

module TypIncludes : sig
  type t

  val create : unit -> t

  class con :
    t
    -> object
         method typ_includes : t
       end
end

module TypImports : sig
  type t

  val create : unit -> t

  class con :
    t
    -> object
         method typ_imports : t
       end
end

module ExpImports : sig
  type t

  class con :
    t
    -> object
         method exp_imports : t
       end

  val create : unit -> t

  val get :
    string ->
    ( < con ; .. >,
      [> Error.t],
      (Exp.Var.t * Exp.Core.t * Typ.Core.t * string list) * Annot.map )
    rea
end

module ImportChain : sig
  type m

  class con :
    object
      method chain : m
    end
end

val elaborate_typ :
  FomCST.Typ.t ->
  ( < Annot.con
    ; Fetch.con
    ; ImportChain.con
    ; Kind.UnkEnv.con
    ; Parameters.con
    ; [`Kind of Kind.t | `Typ of Typ.t] Typ.VarEnv.con
    ; TypImports.con
    ; TypIncludes.con
    ; Typ.Goals.con
    ; Typ.Solved.con
    ; .. >,
    [> Error.t],
    Typ.t )
  rea

val elaborate :
  FomCST.Exp.t ->
  ( < Annot.con
    ; Exp.VarEnv.con
    ; ExpImports.con
    ; Fetch.con
    ; ImportChain.con
    ; Kind.UnkEnv.con
    ; Parameters.con
    ; [`Kind of Kind.t | `Typ of Typ.t] Typ.VarEnv.con
    ; TypImports.con
    ; TypIncludes.con
    ; Typ.Goals.con
    ; Typ.Solved.con
    ; .. >,
    [> Error.t],
    Exp.Core.t * Typ.Core.t * string list )
  rea
