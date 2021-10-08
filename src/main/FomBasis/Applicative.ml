open Higher.Syntax
open Functor.Syntax

type ('a, 'f) return = 'a -> ('a, 'f) app'1
type ('a, 'b, 'f) pair = ('a, 'f) app'1 -> ('b, 'f) app'1 -> ('a * 'b, 'f) app'1

type 'f t =
  < 'f Functor.t
  ; return : 'a. ('a, 'f) return
  ; pair : 'a 'b. ('a, 'b, 'f) pair >

type ('f, 'F, 'a) fr = (< 'f t ; .. > as 'F) -> ('a, 'f) app'1

module Syntax = struct
  let return x : (_, _, _) fr = fun f -> f#return x
  let ( and+ ) xM yM : (_, _, _) fr = fun f -> f#pair (xM f) (yM f)

  (* *)

  let ( <*> ) = ( and+ )

  (* *)

  let unit : (_, _, _) fr = fun f -> f#return ()
  let if_then c uM = if c then uM else unit

  (* *)

  let lift2 xyz x y = x <*> y >>- fun (x, y) -> xyz x y
end
