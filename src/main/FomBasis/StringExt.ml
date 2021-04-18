let is_prefix s1 s2 =
  String.length s1 <= String.length s2
  && s1 = String.sub s2 0 (String.length s1)

let is_suffix s1 s2 =
  let n1 = String.length s1 in
  let n2 = String.length s2 in
  n1 <= n2 && s1 = String.sub s2 (n2 - n1) n1

let drop n s = String.sub s n (String.length s - n)
let drop_last n s = String.sub s 0 (String.length s - n)
let split n s = (String.sub s 0 n, drop n s)

let index_from_opt s i c =
  try Some (String.index_from s i c) with Not_found -> None

let split_on_char c s =
  let rec loop ss i =
    match index_from_opt s i c with
    | None -> String.sub s i (String.length s - i) :: ss
    | Some j -> loop (String.sub s i (j - i) :: ss) (j + 1)
  in
  loop [] 0 |> List.rev
