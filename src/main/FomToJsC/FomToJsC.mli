open Rea
open StdlibPlus
open FomAST
open FomError
open FomElab

module ModSimplified : sig
  type 'R t

  val create : unit -> 'R t

  class ['R] con :
    'R t
    -> object
         method mod_simplified : 'R t
       end
end

module ModInJs : sig
  type 'R t

  val create : unit -> 'R t

  class ['R] con :
    'R t
    -> object
         method mod_in_js : 'R t
       end
end

val to_js :
  whole:bool ->
  top:[`Top | `Body] ->
  Exp.Core.t ->
  string List.t ->
  ( 'R,
    [> Error.t],
    string,
    (< 'R ExpImports.con
     ; 'R ModInJs.con
     ; 'R ModSimplified.con
     ; ('R, 'D) async'
     ; .. >
     as
     'D) )
  er
