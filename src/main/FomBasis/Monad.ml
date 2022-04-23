open Higher.Syntax
open Applicative.Syntax

type ('a, 'b, 'f) bind =
  ('a -> ('b, 'f) app'1) -> ('a, 'f) app'1 -> ('b, 'f) app'1

type 'f t = < 'f Applicative.t ; bind : 'a 'b. ('a, 'b, 'f) bind >
type ('f, 'F, 'a) fr = (< 'f t ; .. > as 'F) -> ('a, 'f) app'1
type ('f, 'a) frm = ('f, 'f t, 'a) fr

module Syntax = struct
  let ( let* ) xM xyM : (_, _, _) fr =
   fun f -> xM f |> f#bind (fun x -> xyM x f)

  let ( and* ) xM yM : (_, _, _) fr = fun f -> f#pair (xM f) (yM f)

  (* *)

  let ( >>= ) = ( let* )
  let ( >> ) uM xM = uM >>= fun () -> xM
  let ( >=> ) abM bcM a = abM a >>= bcM

  (* *)

  let ( &&& ) lhs rhs = lhs >>= fun lhs -> if lhs then rhs else return false
  let ( ||| ) lhs rhs = lhs >>= fun lhs -> if lhs then return true else rhs

  (* *)

  let delay uxM = unit >>= uxM
end
