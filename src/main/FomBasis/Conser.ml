include Monad.Make (struct
  type ('T1, 'r, 'a) t = 'r list -> 'a * 'r list

  let return x rs = (x, rs)

  let ( let* ) xW xyW rs =
    let x, rs = xW rs in
    xyW x rs

  let ( let+ ) xW xy rs =
    let x, rs = xW rs in
    (xy x, rs)
end)

let yield r rs = ((), r :: rs)
let run x = x []
