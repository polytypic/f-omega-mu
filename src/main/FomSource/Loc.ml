open FomBasis

type t = Pos.t * Pos.t

let of_path path =
  let pos = Pos.of_path path in
  (pos, Pos.add_cnum (String.length path) pos)

let dummy = (Lexing.dummy_pos, Lexing.dummy_pos)
let union l r = (fst l, snd r)
let path ((l : Pos.t), _) = l.pos_fname

let compare (ll, lr) (rl, rr) =
  Pos.compare ll rl <>? fun () -> Pos.compare lr rr

let equal l r = compare l r = 0
let is_dummy = equal dummy
let is_empty (l, r) = Pos.equal l r

let pp ((lhs, rhs) : t) =
  let format_range lhs rhs =
    if lhs = rhs then
      Printf.sprintf " %d" lhs
    else
      Printf.sprintf "s %d-%d" lhs rhs
  in
  let file_info =
    if lhs.pos_fname = "" then
      "on"
    else
      Printf.sprintf "in file %s, on"
        (lhs.pos_fname |> JsonString.of_utf8 |> JsonString.to_utf8_json)
  in
  FomPP.utf8format "%s line%s, column%s" file_info
    (format_range lhs.pos_lnum rhs.pos_lnum)
    (format_range (Pos.column_of lhs) (Pos.column_of rhs))

let to_string = pp >>> FomPP.to_string
