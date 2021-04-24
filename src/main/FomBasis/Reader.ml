include Monad.Make (struct
  type ('T1, 'r, 'a) t = 'r -> 'a

  let return x _ = x
  let ( let* ) xM xyM r = xyM (xM r) r
end)

let run r x = x r
let env_as ra r = ra r
let with_env rs xM r = xM (rs r)
