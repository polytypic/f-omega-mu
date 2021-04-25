type 'a uop = 'a -> 'a
type 'a bop = 'a -> 'a -> 'a
type 'a bpr = 'a -> 'a -> bool
type 'a cmp = 'a -> 'a -> int

module Compare = Compare
module Conser = Conser
module Exn = Exn
module Field = Field
module FilenameExt = FilenameExt
module ListExt = ListExt
module Monad = Monad
module Pair = Pair
module Reader = Reader
module StringExt = StringExt
module UTF8 = UTF8

let failwithf = Exn.failwithf

(* *)

let ( <>? ) = Compare.( <>? )

(* *)

let ( >>> ) ab bc a = bc (ab a)
let ( <<< ) bc ab a = bc (ab a)
