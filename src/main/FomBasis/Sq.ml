open Applicative.Syntax

type 'a t = 'a * 'a

let exists pr (x, y) = pr x || pr y
let map_fr fn (x, y) = fn x <*> fn y
