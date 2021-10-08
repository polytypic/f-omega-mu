include Stdlib.String
open Fun.Syntax

let is_prefix s1 s2 = length s1 <= length s2 && s1 = sub s2 0 (length s1)

let is_suffix s1 s2 =
  let n1 = length s1 in
  let n2 = length s2 in
  n1 <= n2 && s1 = sub s2 (n2 - n1) n1

let drop n s = sub s n (length s - n)
let drop_last n s = sub s 0 (length s - n)
let split n s = (sub s 0 n, drop n s)
let index_from_opt s i c = try Some (index_from s i c) with Not_found -> None

let split_on_char c s =
  let rec loop ss i =
    match index_from_opt s i c with
    | None -> sub s i (length s - i) :: ss
    | Some j -> loop (sub s i (j - i) :: ss) (j + 1)
  in
  loop [] 0 |> List.rev

let filter pr = to_seq >>> Seq.filter pr >>> of_seq
