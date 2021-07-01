let prefer_lhs _ l r =
  match (l, r) with Some l, _ -> Some l | _, Some r -> Some r | _, _ -> None

let prefer_rhs _ l r =
  match (l, r) with _, Some r -> Some r | Some l, _ -> Some l | _, _ -> None
