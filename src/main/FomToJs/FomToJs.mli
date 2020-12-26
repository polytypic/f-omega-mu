open FomSyntax

val to_js : Exp.t -> string
(** Transpile Fωμ expression to JavaScript.  Note that this does not type check
    the expression. *)
