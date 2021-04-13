module Exn = Exn
module Compare = Compare
module ListExt = ListExt
module Pair = Pair
module UTF8 = UTF8
module Reader = Reader
module Monad = Monad

let id x = x
let failwithf = Exn.failwithf

(* *)

let ( <>? ) = Compare.( <>? )

(* *)

let ( >>> ) ab bc a = bc (ab a)
let ( <<< ) bc ab a = bc (ab a)
