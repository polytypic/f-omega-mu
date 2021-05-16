open FomBasis
open FomAST

val to_js : Exp.t -> ('r, 'e, string) Rea.t
(** Transpile Fωμ expression to JavaScript.  Note that this does not type check
    the expression. *)
