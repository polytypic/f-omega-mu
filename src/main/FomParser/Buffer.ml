open FomBasis

type t = Sedlexing.lexbuf

let loc = Sedlexing.lexing_positions
let lexeme_utf_8 = Sedlexing.Utf8.lexeme

let init filename lexbuf =
  Sedlexing.set_filename lexbuf filename;
  Sedlexing.set_position lexbuf
    {Lexing.pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0}

let from_utf_8 ?(filename = "") input =
  let lexbuf = input |> UTF8.to_uchar_array |> Sedlexing.from_uchar_array in
  init filename lexbuf;
  lexbuf
