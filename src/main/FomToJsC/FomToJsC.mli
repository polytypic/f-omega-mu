open FomBasis
open FomAST
open FomError
open FomElab

module ModSimplified : sig
  type t

  val create : unit -> t

  class con :
    t
    -> object
         method mod_simplified : t
       end

  type 'r f = con
end

module ModInJs : sig
  type t

  val create : unit -> t

  class con :
    t
    -> object
         method mod_in_js : t
       end

  type 'r f = con
end

val to_js :
  whole:bool ->
  top:[`Top | `Body] ->
  Exp.Core.t ->
  string List.t ->
  ( (< 'r ExpImports.f ; 'r ModSimplified.f ; 'r ModInJs.f ; .. > as 'r),
    [> Error.t],
    string )
  rea
