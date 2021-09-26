include Stdlib.Filename

let canonic path =
  let rec loop p s =
    match (p, s) with
    | _, [] -> p
    | _, "." :: s -> loop p s
    | ("" | "..") :: _, ".." :: s -> loop (".." :: p) s
    | _ :: p, ".." :: s -> loop p s
    | _, n :: s -> loop (n :: p) s
  in
  loop [] (String.split_on_char '/' path) |> List.rev |> String.concat "/"
