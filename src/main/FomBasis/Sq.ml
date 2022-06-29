open Functor.Syntax
open Applicative.Syntax

type 'a t = 'a * 'a

let exists pr (x, y) = pr x || pr y
let map_fr fn (x, y) = fn x <*> fn y

let map_eq_fr fn ((x, y) as xy) =
  fn x <*> fn y >>- fun ((x', y') as xy') ->
  if x == x' && y == y' then xy else xy'
