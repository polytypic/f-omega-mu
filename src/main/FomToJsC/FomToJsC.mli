open StdlibPlus
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
end

module ModInJs : sig
  type t

  val create : unit -> t

  class con :
    t
    -> object
         method mod_in_js : t
       end
end

val to_js :
  whole:bool ->
  top:[`Top | `Body] ->
  Exp.Core.t ->
  string List.t ->
  ( < ExpImports.con ; ModSimplified.con ; ModInJs.con ; .. >,
    [> Error.t],
    string )
  rea
