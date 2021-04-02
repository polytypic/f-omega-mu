type ('e, 'x) t = 'e -> 'x

let run r x = x r
let return x _ = x
let ( let* ) x f e = f (x e) e
let read f = f

let lift1 xy x =
  let* x = x in
  return @@ xy x

let lift2 xyz x y =
  let* x = x in
  let* y = y in
  return @@ xyz x y

let ( &&& ) lhs rhs =
  let* lhs = lhs in
  if lhs then rhs else return false

let ( ||| ) lhs rhs =
  let* lhs = lhs in
  if lhs then return true else rhs

let fold_left yxy y xs =
  let rec loop y = function
    | [] -> return y
    | x :: xs ->
      let* y = yxy y x in
      loop y xs
  in
  loop y xs

let iter f = fold_left (fun _ -> f) ()

let traverse f xs =
  let* ys =
    xs
    |> fold_left
         (fun ys x ->
           let* y = f x in
           return (y :: ys))
         []
  in
  return @@ List.rev ys

let rec for_all p = function
  | [] -> return true
  | x :: xs -> p x &&& for_all p xs

let rec exists p = function
  | [] -> return false
  | x :: xs -> p x ||| exists p xs
