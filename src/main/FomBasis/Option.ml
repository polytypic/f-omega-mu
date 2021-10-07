open Applicative.Syntax
include Stdlib.Option

let iter_fr xF = function None -> unit | Some x -> xF x
