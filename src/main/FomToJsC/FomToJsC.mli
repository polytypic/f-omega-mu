open FomBasis
open FomAST
open FomError
open FomElab

module ModSimplified : sig
  type t

  val create : unit -> t

  class con :
    t
    -> object ('r)
         method mod_simplified : t
       end
end

module ModInJs : sig
  type t

  val create : unit -> t

  class con :
    t
    -> object ('r)
         method mod_in_js : t
       end
end

val to_js :
  whole:bool ->
  Exp.Core.t ->
  string List.t ->
  ( < exp_imports : ExpImports.t
    ; mod_simplified : ModSimplified.t
    ; mod_in_js : ModInJs.t
    ; .. >,
    [> Error.t],
    string )
  rea
