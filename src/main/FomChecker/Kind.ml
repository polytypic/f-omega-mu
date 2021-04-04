open FomDiag

(* *)
include FomAST.Kind

let rec equal lhs rhs =
  match (lhs, rhs) with
  | `Star _, `Star _ -> true
  | `Arrow (_, lhs_dom, lhs_cod), `Arrow (_, rhs_dom, rhs_cod) ->
    equal lhs_dom rhs_dom && equal lhs_cod rhs_cod
  | _ -> false

let check_equal at lhs rhs =
  if not (equal lhs rhs) then
    Error.kind_mismatch at lhs rhs
