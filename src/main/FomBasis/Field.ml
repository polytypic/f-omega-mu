type ('a, 'r) t = 'a * ('a -> 'r)

let make v set = (v, set)
let get the o = fst (the o)

let map the fn o =
  let x, update = the o in
  update (fn x)

let set the v o = snd (the o) v
