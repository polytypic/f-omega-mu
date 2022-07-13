open StdlibPlus

(* *)
include FomAST.Kind

let rec resolve k =
  k
  |> keep_phys_eq_fr @@ function
     | `Star _ as k -> return k
     | `Arrow (at', d, c) ->
       let+ d' = resolve d and+ c' = resolve c in
       `Arrow (at', d', c')
     | `Unk (_, v) as k -> (
       let* k_opt = UnkEnv.find_opt v in
       match k_opt with
       | None -> return k
       | Some k ->
         let* k' = resolve k in
         if k == k' then return k else UnkEnv.add v k' >> return k')

let rec ground k =
  k
  |> keep_phys_eq @@ function
     | `Star _ as k -> k
     | `Arrow (at', d, c) -> `Arrow (at', ground d, ground c)
     | `Unk (at', _) -> `Star at'

let rec occurs_check at' v = function
  | `Star _ -> unit
  | `Arrow (_, d, c) -> occurs_check at' v d >> occurs_check at' v c
  | `Unk (_, v') ->
    if Unk.equal v v' then fail @@ `Error_cyclic_kind at' else unit

let rec unify at' lhs rhs =
  match (lhs, rhs) with
  | `Star _, `Star _ -> unit
  | `Arrow (_, ld, lc), `Arrow (_, rd, rc) ->
    unify at' ld rd
    >> let* lc = resolve lc and* rc = resolve rc in
       unify at' lc rc
  | `Unk (_, l), `Unk (_, r) when Unk.equal l r -> unit
  | `Unk (_, v), t | t, `Unk (_, v) -> occurs_check at' v t >> UnkEnv.add v t
  | _ -> fail @@ `Error_kind_mismatch (at', lhs, rhs)

let unify at' lhs rhs =
  let* lhs = resolve lhs and* rhs = resolve rhs in
  unify at' lhs rhs
