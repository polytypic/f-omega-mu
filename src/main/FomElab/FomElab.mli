open Rea
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

  class ['R, 'D] con :
    object
      method fetch :
        'E.
        Loc.t ->
        string ->
        ('R, ([> e] as 'E), string, (('R, 'D) #fail' as 'D)) er
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
  type 'R t

  val create : unit -> 'R t

  class ['R] con :
    'R t
    -> object
         method typ_includes : 'R t
       end
end

module TypImports : sig
  type 'R t

  val create : unit -> 'R t

  class ['R] con :
    'R t
    -> object
         method typ_imports : 'R t
       end
end

module ExpImports : sig
  type 'R t

  class ['R] con :
    'R t
    -> object
         method exp_imports : 'R t
       end

  val create : unit -> 'R t

  val get :
    string ->
    ( 'R,
      [> Error.t],
      (Exp.Var.t * Exp.Core.t * Typ.Core.t * string list) * Annot.map,
      (< 'R con ; ('R, 'D) async' ; .. > as 'D) )
    er
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
  ( 'R,
    [> Error.t],
    Typ.t,
    (< 'R TypImports.con
     ; 'R TypIncludes.con
     ; ('R, 'D) Fetch.con
     ; ('R, 'D) async'
     ; Annot.con
     ; ImportChain.con
     ; Kind.UnkEnv.con
     ; Parameters.con
     ; Typ.Goals.con
     ; Typ.Solved.con
     ; [`Kind of Kind.t | `Typ of Typ.t] Typ.VarEnv.con
     ; .. >
     as
     'D) )
  er

val elaborate :
  FomCST.Exp.t ->
  ( 'R,
    [> Error.t],
    Exp.Core.t * Typ.Core.t * string list,
    (< 'R ExpImports.con
     ; 'R TypImports.con
     ; 'R TypIncludes.con
     ; ('R, 'D) Fetch.con
     ; ('R, 'D) async'
     ; Annot.con
     ; Exp.VarEnv.con
     ; ImportChain.con
     ; Kind.UnkEnv.con
     ; Parameters.con
     ; Typ.Goals.con
     ; Typ.Solved.con
     ; [`Kind of Kind.t | `Typ of Typ.t] Typ.VarEnv.con
     ; .. >
     as
     'D) )
  er
