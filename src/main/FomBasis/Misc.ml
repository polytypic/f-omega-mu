module Syntax = struct
  type 'a uop = 'a -> 'a
  type 'a bop = 'a -> 'a -> 'a
  type 'a bpr = 'a -> 'a -> bool
  type 'a cmp = 'a -> 'a -> int

  (* *)

  let eq'2 (a, b) (s, t) = a == s && b == t
  let eq'3 (a, b, c) (s, t, u) = a == s && b == t && c == u
  let eq'4 (a, b, c, d) (s, t, u, v) = a == s && b == t && c == u && d == v

  (* *)

  module type OrderedType = Stdlib.Set.OrderedType
end
