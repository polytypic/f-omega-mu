let ( <>? ) lhs rhs = if lhs = 0 then rhs () else lhs
let the part_of compare lhs rhs = compare (part_of lhs) (part_of rhs)

module Tuple'2 (T1 : Set.OrderedType) (T2 : Set.OrderedType) = struct
  type t = T1.t * T2.t

  let compare (l1, l2) (r1, r2) =
    T1.compare l1 r1 <>? fun () -> T2.compare l2 r2
end

module Tuple'3
    (T1 : Set.OrderedType)
    (T2 : Set.OrderedType)
    (T3 : Set.OrderedType) =
struct
  type t = T1.t * T2.t * T3.t

  let compare (l1, l2, l3) (r1, r2, r3) =
    T1.compare l1 r1 <>? fun () ->
    T2.compare l2 r2 <>? fun () -> T3.compare l3 r3
end

module Tuple'4
    (T1 : Set.OrderedType)
    (T2 : Set.OrderedType)
    (T3 : Set.OrderedType)
    (T4 : Set.OrderedType) =
struct
  type t = T1.t * T2.t * T3.t * T4.t

  let compare (l1, l2, l3, l4) (r1, r2, r3, r4) =
    T1.compare l1 r1 <>? fun () ->
    T2.compare l2 r2 <>? fun () ->
    T3.compare l3 r3 <>? fun () -> T4.compare l4 r4
end
