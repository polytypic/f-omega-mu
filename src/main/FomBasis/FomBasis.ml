type 'a uop = 'a -> 'a
type 'a bop = 'a -> 'a -> 'a
type 'a bpr = 'a -> 'a -> bool
type 'a cmp = 'a -> 'a -> int

module Exn = Exn
module Compare = Compare
module ListExt = ListExt
module Pair = Pair
module UTF8 = UTF8
module Reader = Reader
module Conser = Conser
module FilenameExt = FilenameExt
module StringExt = StringExt
module Monad = Monad

let failwithf = Exn.failwithf

(* *)

let ( <>? ) = Compare.( <>? )

(* *)

let ( >>> ) ab bc a = bc (ab a)
let ( <<< ) bc ab a = bc (ab a)
