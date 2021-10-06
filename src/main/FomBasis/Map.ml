module type OrderedType = Stdlib.Map.OrderedType

module type S = sig
  include Stdlib.Map.S

  val add_list : (key * 'v) list -> 'v t -> 'v t
  val of_list : (key * 'v) list -> 'v t
end

module Make (Ord : OrderedType) = struct
  include Stdlib.Map.Make (Ord)

  let add_list kvs = List.to_seq kvs |> add_seq
  let of_list kvs = List.to_seq kvs |> of_seq
end

let prefer_lhs _ l r =
  match (l, r) with Some l, _ -> Some l | _, Some r -> Some r | _, _ -> None

let prefer_rhs _ l r =
  match (l, r) with _, Some r -> Some r | Some l, _ -> Some l | _, _ -> None
