type diagnostic = Loc.t * FomPP.document

exception Error of diagnostic * diagnostic list

let error overview details = raise (Error (overview, details))
