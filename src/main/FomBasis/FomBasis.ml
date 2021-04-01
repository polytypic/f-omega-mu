module Exn = Exn
module Compare = Compare
module ListExt = ListExt
module UTF8 = UTF8
module Reader = Reader

let id x = x
let swap (x, y) = (y, x)
let failwithf = Exn.failwithf

(* *)

let ( <>? ) = Compare.( <>? )

(* *)

let ( >> ) ab bc a = bc (ab a)
let ( << ) bc ab a = bc (ab a)
