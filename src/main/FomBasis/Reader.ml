include Monad.Make (struct
  type ('e, 'x) t = 'e -> 'x

  let return x _ = x
  let ( let* ) x f e = f (x e) e
end)

let run r x = x r
let read f = f
