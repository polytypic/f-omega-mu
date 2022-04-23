open Higher.Syntax
open Functor.Syntax

type ('a, 'f) return = 'a -> ('a, 'f) app'1
type ('a, 'b, 'f) pair = ('a, 'f) app'1 -> ('b, 'f) app'1 -> ('a * 'b, 'f) app'1

type 'f t =
  < 'f Functor.t
  ; return : 'a. ('a, 'f) return
  ; pair : 'a 'b. ('a, 'b, 'f) pair >

type ('f, 'F, 'a) fr = (< 'f t ; .. > as 'F) -> ('a, 'f) app'1
type ('f, 'a) frm = ('f, 'f t, 'a) fr

module Syntax = struct
  let return x : (_, _, _) fr = fun f -> f#return x
  let ( and+ ) xM yM : (_, _, _) fr = fun f -> f#pair (xM f) (yM f)

  (* *)

  let ( <*> ) = ( and+ )

  (* *)

  let tuple'2 = ( and+ )

  let tuple'3 xM yM zM =
    let+ x = xM and+ y = yM and+ z = zM in
    (x, y, z)

  (* *)

  let unit : (_, _, _) fr = fun f -> f#return ()
  let do_unless c uM = if c then unit else uM
  let do_when c uM = if c then uM else unit

  (* *)

  let lift2 xyz x y = x <*> y >>- fun (x, y) -> xyz x y

  (* *)

  let thunk u2x = unit >>- u2x
end
