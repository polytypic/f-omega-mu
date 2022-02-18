open Higher.Syntax

type ('a, 'f) zero = ('a, 'f) app'1
type ('a, 'f) alt = ('a, 'f) app'1 -> ('a, 'f) app'1 -> ('a, 'f) app'1
type 'f t = < zero : 'a. ('a, 'f) zero ; alt : 'a. ('a, 'f) alt >
type ('f, 'F, 'a) fr = (< 'f t ; .. > as 'F) -> ('a, 'f) app'1

module Syntax : sig
  val zero : ('f, 'F, 'a) fr
  val ( <|> ) : ('f, 'F, 'a) fr -> ('f, 'F, 'a) fr -> ('f, 'F, 'a) fr
end
