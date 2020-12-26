let ( <>? ) lhs rhs = if lhs = 0 then rhs () else lhs
let the part_of compare lhs rhs = compare (part_of lhs) (part_of rhs)

module Pair (Fst : Set.OrderedType) (Snd : Set.OrderedType) = struct
  type t = Fst.t * Snd.t

  let compare lhs rhs =
    Fst.compare (fst lhs) (fst rhs) <>? fun () ->
    Snd.compare (snd lhs) (snd rhs)
end
