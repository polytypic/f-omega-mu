open Higher.Syntax

type ('a, 'f) zero = ('a, 'f) app'1
type ('a, 'f) alt = ('a, 'f) app'1 -> ('a, 'f) app'1 -> ('a, 'f) app'1
type 'f t = < zero : 'a. ('a, 'f) zero ; alt : 'a. ('a, 'f) alt >
type ('f, 'F, 'a) fr = (< 'f t ; .. > as 'F) -> ('a, 'f) app'1

module Syntax = struct
  let zero : (_, _, _) fr = fun f -> f#zero
  let ( <|> ) lA rA : (_, _, _) fr = fun f -> f#alt (lA f) (rA f)
end
