open Higher.Syntax

type ('a, 'b, 'f) map = ('a -> 'b) -> ('a, 'f) app'1 -> ('b, 'f) app'1
type 'f t = < map : 'a 'b. ('a, 'b, 'f) map >
type ('f, 'F, 'a) fr = (< 'f t ; .. > as 'F) -> ('a, 'f) app'1

module Syntax = struct
  let ( let+ ) xM xy : (_, _, _) fr = fun f -> xM f |> f#map xy

  (* *)

  let ( >>- ) = ( let+ )
  let ( >-> ) abM bc a = abM a >>- bc

  (* *)

  let lift1 xy xF = xF >>- xy
end
