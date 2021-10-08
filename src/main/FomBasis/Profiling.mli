open Misc.Syntax

module Counter : sig
  type t

  val register : string -> t
  val inc : t -> unit
  val reset_all : unit -> unit
  val dump_all : unit -> unit

  (* *)

  val wrap'1 : string -> ('a -> 'b) uop
  val wrap'2 : string -> ('a -> 'b -> 'c) uop
  val wrap'3 : string -> ('a -> 'b -> 'c -> 'd) uop
  val wrap'4 : string -> ('a -> 'b -> 'c -> 'd -> 'e) uop
end
