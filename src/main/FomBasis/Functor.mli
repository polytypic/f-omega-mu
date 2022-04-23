open Higher.Syntax

type ('a, 'b, 'f) map = ('a -> 'b) -> ('a, 'f) app'1 -> ('b, 'f) app'1
type 'f t = < map : 'a 'b. ('a, 'b, 'f) map >
type ('f, 'F, 'a) fr = (< 'f t ; .. > as 'F) -> ('a, 'f) app'1
type ('f, 'a) frm = ('f, 'f t, 'a) fr

module Syntax : sig
  val ( let+ ) : ('f, 'F, 'a) fr -> ('a -> 'b) -> ('f, 'F, 'b) fr

  (* *)

  val ( >>- ) : ('f, 'F, 'a) fr -> ('a -> 'b) -> ('f, 'F, 'b) fr
  val ( >-> ) : ('a -> ('f, 'F, 'b) fr) -> ('b -> 'c) -> 'a -> ('f, 'F, 'c) fr

  (* *)

  val lift1 : ('a -> 'b) -> ('f, 'F, 'a) fr -> ('f, 'F, 'b) fr
end
