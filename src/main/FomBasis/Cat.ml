type 'a t = Nil | One of 'a | Cat of 'a t * 'a t

let empty = Nil
let singleton x = One x
let append l r = match (l, r) with Nil, x | x, Nil -> x | l, r -> Cat (l, r)

let rec rev_append t ys =
  match t with
  | Nil -> ys
  | One x -> x :: ys
  | Cat (l, r) -> rev_append l (rev_append r ys)

let to_list t = rev_append t []
