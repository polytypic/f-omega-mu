open Rea

(* *)

module Lam = struct
  include Lam
  module Var = LamToJs.Var
end

let in_env op =
  op
  |> mapping_env @@ fun o ->
     object
       inherit [_, _, _] async'of o
       inherit Lam.Env.con
       inherit Lam.Limit.con
       inherit Lam.Seen.con
       inherit Lam.Renumbering.con
     end

let erase = FomToLam.erase

let simplify exp =
  exp |> LamSimplify.to_fixed_point >>= LamHoist.constants_to_top |> in_env

let to_js ~top exp =
  LamToJs.to_js_stmts
    (top :> [`Body | `Return | `Seq | `Tail of _ | `Top])
    Lam.VarSet.empty exp
  |> in_env
