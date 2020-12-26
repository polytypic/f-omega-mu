let to_uchar_array str =
  let buffer = Array.make (Stdlib.String.length str) Uchar.min in
  let n =
    str
    |> Uutf.String.fold_utf_8
         (fun i _ -> function
           | `Uchar c ->
             buffer.(i) <- c;
             i + 1
           | `Malformed _ -> Exn.failwithf "Malformed UTF-8 at char index %d" i)
         0
  in
  if n <> Array.length buffer then
    Array.sub buffer 0 n
  else
    buffer

let of_uchar_array chars =
  let buffer = Buffer.create (Array.length chars * 2) in
  let encoder = Uutf.encoder `UTF_8 (`Buffer buffer) in
  chars |> Array.iter (fun char -> Uutf.encode encoder (`Uchar char) |> ignore);
  Uutf.encode encoder `End |> ignore;
  Buffer.contents buffer
