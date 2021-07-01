type 'a uop = 'a -> 'a
type 'a bop = 'a -> 'a -> 'a
type 'a bpr = 'a -> 'a -> bool
type 'a cmp = 'a -> 'a -> int

module Compare = Compare
module Exn = Exn
module Field = Field
module FilenameExt = FilenameExt
module IVar = IVar
module ListExt = ListExt
module MapExt = MapExt
module Monad = Monad
module MVar = MVar
module Pair = Pair
module Rea = Rea
module Res = Res
module StringExt = StringExt
module UTF8 = UTF8
module Zero = Zero

let failwithf = Exn.failwithf

(* *)

let ( <>? ) = Compare.( <>? )

(* *)

let ( >>> ) ab bc a = bc (ab a)
let ( <<< ) bc ab a = bc (ab a)
