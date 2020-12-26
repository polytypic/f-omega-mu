type ('e, 'x) t = 'e -> 'x

let run r x = x r
let return x _ = x
let ( let* ) x f e = f (x e) e
let read f = f
let bind_in f x e = x (f e)

let traverse f xs =
  let rec loop ys = function
    | [] -> return (List.rev ys)
    | x :: xs ->
      let* y = f x in
      loop (y :: ys) xs
  in
  loop [] xs
