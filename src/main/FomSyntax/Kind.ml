open FomBasis
open FomSource

(* *)

type t = [`Star of Loc.t | `Arrow of Loc.t * t * t]

let at = function `Star at -> at | `Arrow (at, _, _) -> at

(* Comparison *)

let index = function `Star _ -> 0 | `Arrow _ -> 1

let rec compare lhs rhs =
  match (lhs, rhs) with
  | `Star _, `Star _ -> 0
  | `Arrow (_, lhs_dom, lhs_cod), `Arrow (_, rhs_dom, rhs_cod) ->
    compare lhs_dom rhs_dom <>? fun () -> compare lhs_cod rhs_cod
  | _ -> index lhs - index rhs

(* Formatting *)

let rec pp atomize kind =
  let open FomPP in
  match kind with
  | `Star _ -> star
  | `Arrow (_, dom, cod) ->
    [pp true dom; break_1; arrow_right; space; pp false cod]
    |> concat |> parens_if atomize

let pp kind = pp false kind |> FomPP.group
