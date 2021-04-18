open FomBasis

type t = Lexing.position

let of_filename filename = {Lexing.dummy_pos with pos_fname = filename}
let column_of {Lexing.pos_cnum; pos_bol; _} = pos_cnum - pos_bol + 1

let compare (l : t) (r : t) =
  String.compare l.pos_fname r.pos_fname <>? fun () ->
  Int.compare l.pos_lnum r.pos_lnum <>? fun () ->
  Int.compare l.pos_cnum r.pos_cnum
