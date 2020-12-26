module Exn = Exn
module Compare = Compare
module ListExt = ListExt
module UTF8 = UTF8
module Reader = Reader

let failwithf = Exn.failwithf

(* *)

let ( <>? ) = Compare.( <>? )

(* *)

let ( >> ) ab bc a = bc (ab a)
let ( << ) bc ab a = bc (ab a)
