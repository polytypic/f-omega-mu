open Monad.Syntax
open Compare.Syntax

(* *)

include Stdlib.List

let rec for_alli p i = function
  | x :: xs -> p i x && for_alli p (i + 1) xs
  | [] -> true

let for_alli p = for_alli p 0

let equal_with equal xs ys =
  try for_all2 equal xs ys with Invalid_argument _ -> false

let rec compare_with compare xs ys =
  match (xs, ys) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
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
  | _ -> raise @@ Invalid_argument "List.share_phys_eq"

let find_dup_opt cmp xs =
  let rec loop = function
    | x1 :: (x2 :: _ as xs) -> if 0 = cmp x1 x2 then Some (x1, x2) else loop xs
    | _ -> None
  in
  xs |> Stdlib.List.stable_sort cmp |> loop

(* *)

let rec fold_left_fr xyx x = function
  | [] -> return x
  | y :: ys -> xyx x y >>= fun x -> fold_left_fr xyx x ys

let rec fold_left2_fr xyzx x ys zs =
  match (ys, zs) with
  | y :: ys, z :: zs -> xyzx x y z >>= fun x -> fold_left2_fr xyzx x ys zs
  | [], [] -> return x
  | _ -> raise @@ Invalid_argument "fold_left2_fr"

(* *)

let rec iter_fr xy = function
  | x :: xs -> xy x >>= fun () -> iter_fr xy xs
  | [] -> unit

let iter_fr_ = iter_fr
let iter2_fr yzu = fold_left2_fr (Fun.const yzu) ()

let rec iter2_fr xyuF xs ys =
  match (xs, ys) with
  | x :: xs, y :: ys -> xyuF x y >>= fun () -> iter2_fr xyuF xs ys
  | [], [] -> unit
  | _, _ -> raise @@ Invalid_argument "iter2_fr"

(* *)

let rec for_all_fr p = function
  | x :: xs -> p x &&& for_all_fr p xs
  | [] -> return true

let rec exists_fr p = function
  | x :: xs -> p x ||| exists_fr p xs
  | [] -> return false

(* *)

let rec find_opt_fr p = function
  | x :: xs -> p x >>= fun b -> if b then return @@ Some x else find_opt_fr p xs
  | [] -> return None

(* *)

let rec map_fr xyF ysF = function
  | x :: xs -> map_fr xyF (xyF x <*> ysF >>- fun (y, ys) -> y :: ys) xs
  | [] -> ysF >>- rev

let map_fr xyF = map_fr xyF (return [])

let rec map_phys_eq_fr fn inn =
  match inn with
  | x :: xs as inn ->
    let+ x' = fn x and+ xs' = map_phys_eq_fr fn xs in
    if x == x' && xs == xs' then inn else x' :: xs'
  | [] -> return inn