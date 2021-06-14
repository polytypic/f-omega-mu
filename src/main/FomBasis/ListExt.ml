open Compare

let for_alli p =
  let rec loop i = function
    | [] -> true
    | x :: xs -> p i x && loop (i + 1) xs
  in
  loop 0

let equal_with equal xs ys =
  try List.for_all2 equal xs ys with Invalid_argument _ -> false

let rec compare_with compare xs ys =
  match (xs, ys) with
  | [], [] -> 0
  | [], _ -> 1
  | _, [] -> -1
  | x :: xs, y :: ys -> compare x y <>? fun () -> compare_with compare xs ys

let rec map_phys_eq fn inn =
  match inn with
  | [] -> inn
  | x :: xs as inn ->
    let x' = fn x in
    let xs' = map_phys_eq fn xs in
    if x == x' && xs == xs' then inn else x' :: xs'

let rec share_phys_eq share_phys_eq_elem original changed =
  match (original, changed) with
  | [], [] -> original
  | o :: os, c :: cs ->
    let cs = share_phys_eq share_phys_eq_elem os cs in
    let c = share_phys_eq_elem o c in
    if os == cs && o == c then original else c :: cs
  | _ -> raise @@ Invalid_argument "ListExt.share_phys_eq"
