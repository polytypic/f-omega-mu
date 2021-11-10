open FomBasis

(* *)

module Lam = Lam

let in_env () =
  with_env @@ fun _ ->
  object
    inherit Lam.VarMap.con
    inherit Lam.Limit.con
    inherit Lam.Seen.con
  end

let erase = FomToLam.erase

let simplify exp =
  exp |> LamSimplify.to_fixed_point |> in_env () >>- LamHoist.constants_to_top

let to_js ?(top = `Top) exp =
  LamToJs.to_js_stmts top Lam.VarSet.empty exp |> in_env ()
