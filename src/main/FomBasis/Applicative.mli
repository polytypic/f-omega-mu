open Higher.Syntax

type ('a, 'f) return = 'a -> ('a, 'f) app'1
type ('a, 'b, 'f) pair = ('a, 'f) app'1 -> ('b, 'f) app'1 -> ('a * 'b, 'f) app'1

type 'f t =
  < 'f Functor.t
  ; return : 'a. ('a, 'f) return
  ; pair : 'a 'b. ('a, 'b, 'f) pair >

type ('f, 'F, 'a) fr = (< 'f t ; .. > as 'F) -> ('a, 'f) app'1
type ('f, 'a) frm = ('f, 'f t, 'a) fr

module Syntax : sig
  val return : 'a -> ('f, 'F, 'a) fr
  val ( and+ ) : ('f, 'F, 'a) fr -> ('f, 'F, 'b) fr -> ('f, 'F, 'a * 'b) fr

  (* *)

  val ( <*> ) : ('f, 'F, 'a) fr -> ('f, 'F, 'b) fr -> ('f, 'F, 'a * 'b) fr

  (* *)

  val tuple'2 : ('f, 'F, 'a) fr -> ('f, 'F, 'b) fr -> ('f, 'F, 'a * 'b) fr

  val tuple'3 :
    ('f, 'F, 'a) fr ->
    ('f, 'F, 'b) fr ->
    ('f, 'F, 'c) fr ->
    ('f, 'F, 'a * 'b * 'c) fr

  (* *)

  val unit : ('f, 'F, unit) fr
  val do_unless : bool -> ('f, 'F, unit) fr -> ('f, 'F, unit) fr
  val do_when : bool -> ('f, 'F, unit) fr -> ('f, 'F, unit) fr

  (* *)

  val lift2 :
    ('a -> 'b -> 'c) -> ('f, 'F, 'a) fr -> ('f, 'F, 'b) fr -> ('f, 'F, 'c) fr

  (* *)

  val thunk : (unit -> 'a) -> ('f, 'F, 'a) fr
end
