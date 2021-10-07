type 'a uop = 'a -> 'a
type 'a bop = 'a -> 'a -> 'a
type 'a bpr = 'a -> 'a -> bool
type 'a cmp = 'a -> 'a -> int

module Higher = Higher
module Applicative = Applicative
module Cats = Cats
module Compare = Compare
module Constant = Constant
module Exn = Exn
module Field = Field
module Filename = Filename
module Fun = Fun
module Functor = Functor
module Identity = Identity
module IVar = IVar
module JsonString = JsonString
module LVar = LVar
module List = List
module Map = Map
module Monad = Monad
module Monoid = Monoid
module MVar = MVar
module Option = Option
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

(* *)

let eq'2 (a, b) (s, t) = a == s && b == t
let eq'3 (a, b, c) (s, t, u) = a == s && b == t && c == u
let eq'4 (a, b, c, d) (s, t, u, v) = a == s && b == t && c == u && d == v

(* *)

include Higher.Syntax
include Monad.Syntax
include Rea.Syntax
include Compare.Syntax
include Exn.Syntax
include Fun.Syntax
