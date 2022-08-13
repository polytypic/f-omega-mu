open Rea
open StdlibPlus

(* *)
include FomAST.Kind

let rec resolve k =
  k
  |> keep_eq_er @@ function
     | `Star _ as k -> pure k
     | `Arrow (at', d, c) ->
       let+ d' = resolve d and+ c' = resolve c in
       `Arrow (at', d', c')
     | `Unk (_, v) as k -> (
       UnkEnv.find_opt v >>= function
       | None -> pure k
       | Some k ->
         let* k' = resolve k in
         if k == k' then pure k else UnkEnv.add v k' >> pure k')

let rec ground k =
  k
  |> keep_eq @@ function
     | `Star _ as k -> k
     | `Arrow (at', d, c) -> `Arrow (at', ground d, ground c)
     | `Unk (at', _) -> `Star at'

let rec occurs_check at' v =
  eta'1 @@ function
  | `Star _ -> unit
  | `Arrow (_, d, c) -> occurs_check at' v d >> occurs_check at' v c
  | `Unk (_, v') ->
    if Unk.equal v v' then fail @@ `Error_cyclic_kind at' else unit

let rec unify at' lhs rhs =
  eta'0 @@ fun () ->
  match (lhs, rhs) with
  | `Star _, `Star _ -> unit
  | `Arrow (_, ld, lc), `Arrow (_, rd, rc) ->
    unify at' ld rd >> (resolve lc <*> resolve rc >>= uncurry @@ unify at')
  | `Unk (_, l), `Unk (_, r) when Unk.equal l r -> unit
  | `Unk (_, v), t | t, `Unk (_, v) -> occurs_check at' v t >> UnkEnv.add v t
  | _ -> fail @@ `Error_kind_mismatch (at', lhs, rhs)

let unify at' lhs rhs = resolve lhs <*> resolve rhs >>= uncurry @@ unify at'
