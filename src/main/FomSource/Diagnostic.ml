type t = Loc.t * FomPP.document

exception Error of t * t list

let error overview details = raise @@ Error (overview, details)
