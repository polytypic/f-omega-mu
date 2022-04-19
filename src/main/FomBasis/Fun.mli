include module type of Stdlib.Fun

module Syntax : sig
  val id : 'a -> 'a
  val const : 'a -> 'b -> 'a
  val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
  val ( >>> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
  val ( <<< ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
end
