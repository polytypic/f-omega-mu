include Monad.Make (struct
  type ('r, 'T2, 'a) t = 'r -> 'a

  let return x _ = x
  let ( let* ) xM xyM r = xyM (xM r) r
  let ( let+ ) xM xy r = xy (xM r)
end)

let run r x = x r

(* *)

let env_as ra r = ra r
let with_env rs xM r = xM (rs r)

(* *)

let get the r = Field.get the r
let get_as the fg r = fg @@ Field.get the r
let setting the v xM r = xM @@ Field.set the v r
let mapping the vv xM r = xM @@ Field.map the vv r
