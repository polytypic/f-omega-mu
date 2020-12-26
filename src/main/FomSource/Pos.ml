type t = Lexing.position

let column_of {Lexing.pos_cnum; pos_bol; _} = pos_cnum - pos_bol + 1
