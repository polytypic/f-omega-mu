open FomBasis

(* *)

module Lam = struct
  include Lam
  module Var = LamToJs.Var
end

let in_env () =
  with_env @@ fun _ ->
  object
    inherit Lam.Env.con
    inherit Lam.Limit.con
    inherit Lam.Seen.con
    inherit Lam.Renumbering.con
  end

let erase = FomToLam.erase

let simplify exp =
  exp |> LamSimplify.to_fixed_point |> in_env () >>- LamHoist.constants_to_top

let to_js ~top exp = LamToJs.to_js_stmts top Lam.VarSet.empty exp |> in_env ()
