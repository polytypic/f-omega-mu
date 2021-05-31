open FomBasis

(* *)
include FomAST.Kind
open Rea

module Env = struct
  include Env

  type t = FomAST.Kind.t Env.t ref

  let create () = ref empty
  let field r = r#kind_env

  let find_opt i =
    let* env = env_as field in
    return @@ Env.find_opt i !env

  let add i k =
    let+ env = env_as field in
    env := Env.add i k !env

  class con (env : t) =
    object
      method kind_env = env
    end
end

let rec resolve = function
  | `Star _ as k -> return k
  | `Arrow (at', d, c) as k ->
    let+ d' = resolve d and+ c' = resolve c in
    if d == d' && c == c' then k else `Arrow (at', d', c')
  | `Var (_, v) as k -> (
    let* k_opt = Env.find_opt v in
    match k_opt with
    | None -> return k
    | Some k ->
      let* k' = resolve k in
      if k == k' then
        return k
      else
        Env.add v k' >> return k')

let rec occurs_check at' v = function
  | `Star _ -> unit
  | `Arrow (_, d, c) -> occurs_check at' v d >> occurs_check at' v c
  | `Var (_, v') ->
    if Id.equal v v' then fail @@ `Error_cyclic_kind at' else unit

let rec unify at' lhs rhs =
  match (lhs, rhs) with
  | `Star _, `Star _ -> unit
  | `Arrow (_, ld, lc), `Arrow (_, rd, rc) ->
    unify at' ld rd
    >> let* lc = resolve lc and* rc = resolve rc in
       unify at' lc rc
  | `Var (_, l), `Var (_, r) when Id.equal l r -> unit
  | `Var (_, v), t | t, `Var (_, v) -> occurs_check at' v t >> Env.add v t
  | _ -> fail @@ `Error_kind_mismatch (at', lhs, rhs)

let unify at' lhs rhs =
  let* lhs = resolve lhs and* rhs = resolve rhs in
  unify at' lhs rhs
