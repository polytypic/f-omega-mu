let swap (x, y) = (y, x)
let map f g (x, y) = (f x, g y)

let map_phys_eq f g ((x, y) as inn) =
  let x' = f x in
  let y' = g y in
  if x == x' && y == y' then inn else (x', y')
