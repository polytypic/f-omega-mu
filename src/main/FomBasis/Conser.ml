include Monad.Make (struct
  type ('r, 'a) t = 'r list -> 'a * 'r list

  let return x rs = (x, rs)

  let ( let* ) (xW : ('r, 'x) t) (xyW : 'x -> ('r, 'y) t) : ('r, 'y) t =
   fun rs ->
    let x, rs = xW rs in
    xyW x rs
end)

let yield r rs = ((), r :: rs)
let run x = x []
