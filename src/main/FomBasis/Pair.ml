open Functor.Syntax
open Applicative.Syntax

let set_fst (_, y) x = (x, y)
let set_snd (x, _) y = (x, y)
let swap (x, y) = (y, x)
let map f g (x, y) = (f x, g y)

let map_phys_eq f g ((x, y) as inn) =
  let x' = f x in
  let y' = g y in
  if x == x' && y == y' then inn else (x', y')

let share_phys_eq share_l share_r ((o_l, o_r) as o) (l, r) =
  let l = share_l o_l l in
  let r = share_r o_r r in
  if o_l == l && o_r == r then o else (l, r)

let map_fr xyF zwF (x, z) =
  let+ y = xyF x and+ w = zwF z in
  (y, w)

let map_phys_eq_fr aaM bbM ((a, b) as ab) =
  let+ a' = aaM a and+ b' = bbM b in
  if a == a' && b == b' then ab else (a', b')
