open FomBasis
open FomAST
open FomError
open FomElab

val to_js :
  whole:bool ->
  Exp.Core.t ->
  string List.t ->
  (< exp_imports : ExpImports.t ; .. >, [> Error.t], string) rea
