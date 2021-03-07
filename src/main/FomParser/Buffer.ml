open FomBasis
open FomSource

type t = {
  lexbuf : Sedlexing.lexbuf;
  mutable comments : (Loc.t * Comment.t) list;
}

let loc {lexbuf; _} = Sedlexing.lexing_positions lexbuf
let lexeme_utf_8 {lexbuf; _} = Sedlexing.Utf8.lexeme lexbuf

let init filename lexbuf =
  Sedlexing.set_filename lexbuf filename;
  Sedlexing.set_position lexbuf
    {Lexing.pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0}

let from_utf_8 ?(filename = "") input =
  let lexbuf = input |> UTF8.to_uchar_array |> Sedlexing.from_uchar_array in
  init filename lexbuf;
  {lexbuf; comments = []}

let comments {comments; _} = List.rev comments
