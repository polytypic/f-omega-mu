open StdlibPlus

type state = [`Initial | `Open | `TstrStr | `TstrEsc | `TstrExp] list
type t = {mutable state : state; lexbuf : Sedlexing.lexbuf}

let loc {lexbuf; _} = Sedlexing.lexing_positions lexbuf
let lexeme_utf_8 {lexbuf; _} = Sedlexing.Utf8.lexeme lexbuf

let init path lexbuf =
  Sedlexing.set_filename lexbuf path;
  Sedlexing.set_position lexbuf
    {Lexing.pos_fname = path; pos_lnum = 1; pos_bol = 0; pos_cnum = 0}

let from_utf_8 ?(path = "") input =
  let lexbuf = input |> UTF.UTF8.to_uchar_array |> Sedlexing.from_uchar_array in
  init path lexbuf;
  {state = []; lexbuf}
