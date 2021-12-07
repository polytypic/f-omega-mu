include Stdlib.Array

let choose xyO xs =
  xs
  |> map (fun x -> match xyO x with None -> [||] | Some x -> [|x|])
  |> to_list |> concat

let sorted cmp xs =
  let xs = copy xs in
  sort cmp xs;
  xs
