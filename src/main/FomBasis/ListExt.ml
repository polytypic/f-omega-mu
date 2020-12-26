open Compare

let equal_with equal xs ys =
  try List.for_all2 equal xs ys with Invalid_argument _ -> false

let rec compare_with compare xs ys =
  match (xs, ys) with
  | [], [] -> 0
  | [], _ -> 1
  | _, [] -> -1
  | x :: xs, y :: ys -> compare x y <>? fun () -> compare_with compare xs ys
