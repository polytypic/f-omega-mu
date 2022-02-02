include Stdlib.Array

let choose xyO xs =
  xs
  |> map (fun x -> match xyO x with None -> [||] | Some x -> [|x|])
  |> to_list |> concat

let sorted cmp xs =
  let xs = copy xs in
  sort cmp xs;
  xs

let binary_search_opt cmp xs =
  let rec loop lo hi =
    match hi - lo with
    | 0 -> None
    | 1 ->
      let x = get xs lo in
      if cmp x = 0 then Some x else None
    | n ->
      let mid = lo + (n / 2) in
      let x = get xs mid in
      let dir = cmp x in
      if dir < 0 then loop lo mid
      else if dir > 0 then loop (mid + 1) hi
      else Some x
  in
  loop 0 (length xs)
