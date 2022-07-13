open StdlibPlus
include Loc
open FomPPrint

let pp ((lhs, rhs) : t) =
  let format_range lhs rhs =
    if lhs = rhs then Printf.sprintf " %d" lhs
    else Printf.sprintf "s %d-%d" lhs rhs
  in
  let file_info =
    if lhs.pos_fname = "" then empty
    else
      let filename =
        lhs.pos_fname |> JsonString.of_utf8 |> JsonString.to_utf8_json
        |> utf8string
      in
      text "in file"
      ^^ group (nest 2 (break_1_0 ^^ filename) ^^ comma_break_1_or_break_0_0)
  in
  file_info
  ^^ textf "on line%s, column%s"
       (format_range lhs.pos_lnum rhs.pos_lnum)
       (format_range (Pos.column_of lhs) (Pos.column_of rhs))

let to_string = pp >>> to_string
