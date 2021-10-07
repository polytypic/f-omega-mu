type ('x, 'f) t

module Syntax = struct
  type ('x, 'f) app'1 = ('x, 'f) t
  type ('x, 'y, 'f) app'2 = ('y, ('x, 'f) t) t
  type ('x, 'y, 'z, 'f) app'3 = ('z, ('y, ('x, 'f) t) t) t
end

open Syntax

(* *)

module type T'1 = sig
  type 'a t
end

module type F'1 = sig
  type f
  type 'a t'1

  val inj : 'a t'1 -> ('a, f) app'1
  val prj : ('a, f) app'1 -> 'a t'1
end

module New'1 (T'1 : T'1) () : F'1 with type 'a t'1 = 'a T'1.t = struct
  type f
  type 'a t'1 = 'a T'1.t

  external inj : 'a t'1 -> ('a, f) app'1 = "%identity"
  external prj : ('a, f) app'1 -> 'a t'1 = "%identity"
end

(* *)

module type T'2 = sig
  type ('a, 'b) t
end

module type F'2 = sig
  type f
  type ('a, 'b) t'2

  val inj : ('a, 'b) t'2 -> ('a, 'b, f) app'2
  val prj : ('a, 'b, f) app'2 -> ('a, 'b) t'2
end

module New'2 (T'2 : T'2) () : F'2 with type ('a, 'b) t'2 = ('a, 'b) T'2.t =
struct
  type f
  type ('a, 'b) t'2 = ('a, 'b) T'2.t

  external inj : ('a, 'b) t'2 -> ('a, 'b, f) app'2 = "%identity"
  external prj : ('a, 'b, f) app'2 -> ('a, 'b) t'2 = "%identity"
end

(* *)

module type T'3 = sig
  type ('a, 'b, 'c) t
end

module type F'3 = sig
  type f
  type ('a, 'b, 'c) t'3

  val inj : ('a, 'b, 'c) t'3 -> ('a, 'b, 'c, f) app'3
  val prj : ('a, 'b, 'c, f) app'3 -> ('a, 'b, 'c) t'3
end

module New'3 (T'3 : T'3) () :
  F'3 with type ('a, 'b, 'c) t'3 = ('a, 'b, 'c) T'3.t = struct
  type f
  type ('a, 'b, 'c) t'3 = ('a, 'b, 'c) T'3.t

  external inj : ('a, 'b, 'c) t'3 -> ('a, 'b, 'c, f) app'3 = "%identity"
  external prj : ('a, 'b, 'c, f) app'3 -> ('a, 'b, 'c) t'3 = "%identity"
end
