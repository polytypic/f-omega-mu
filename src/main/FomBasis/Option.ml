open Applicative.Syntax
include Stdlib.Option

let iter_fr xF = function None -> unit | Some x -> xF x
let or_else r = function None -> r () | some -> some
let both f l r = match (l, r) with Some l, Some r -> Some (f l r) | _ -> None
