type 'a uop = 'a -> 'a
type 'a bop = 'a -> 'a -> 'a
type 'a bpr = 'a -> 'a -> bool
type 'a cmp = 'a -> 'a -> int

module Cats = Cats
module Compare = Compare
module Exn = Exn
module Field = Field
module Filename = Filename
module IVar = IVar
module JsonString = JsonString
module LVar = LVar
module List = List
module Map = Map
module Monad = Monad
module MVar = MVar
module Pair = Pair
module Profiling = Profiling
module Rea = Rea
module Res = Res
module String = String

module UTF8 = struct
  let of_uchar_array = UTF.to_utf8
  let to_uchar_array = UTF.to_uchar_array
end

module UTF16 = struct
  let of_uchar_array = UTF.to_utf16_bytes
end

module Zero = Zero

let failwithf = Exn.failwithf

(* *)

let ( <>? ) = Compare.( <>? )

(* *)

let ( >>> ) ab bc a = bc (ab a)
let ( <<< ) bc ab a = bc (ab a)
