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

let encode_as encoding chars =
  let buffer = Buffer.create (Array.length chars * 2) in
  let encoder = Uutf.encoder encoding @@ `Buffer buffer in
  (chars
  |> Array.iter @@ fun char -> ignore @@ Uutf.encode encoder @@ `Uchar char);
  Uutf.encode encoder `End |> ignore;
  buffer

let to_utf16_bytes chars = chars |> encode_as `UTF_16 |> Buffer.to_bytes
let to_utf8 chars = chars |> encode_as `UTF_8 |> Buffer.contents
