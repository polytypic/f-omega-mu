open FomBasis

(* *)
include FomAST.Kind

module UnkMap = struct
  include UnkMap

  type nonrec t = FomAST.Kind.t t MVar.t

  let empty () = MVar.create empty
  let field r = r#kind_env
  let resetting op = setting field (empty ()) op
  let find_opt i = read field >>- UnkMap.find_opt i
  let add i k = mutate field @@ UnkMap.add i k
  let cloning op = read field >>= fun v -> setting field (MVar.create v) op

  class con =
    object
      val kind_env : t = empty ()
      method kind_env = Field.make kind_env (fun v -> {<kind_env = v>})
    end
end

let rec resolve k =
  let+ k' =
    match k with
    | `Star _ as k -> return k
    | `Arrow (at', d, v, c) ->
      let+ d' = resolve d and+ c' = resolve c in
      `Arrow (at', d', v, c')
    | `Unk (_, v) as k -> (
      let* k_opt = UnkMap.find_opt v in
      match k_opt with
      | None -> return k
      | Some k ->
        let* k' = resolve k in
        if k == k' then
          return k
        else
          UnkMap.add v k' >> return k')
  in
  keep_phys_eq' k k'

let rec ground k =
  k
  |> keep_phys_eq @@ function
     | `Star _ as k -> k
     | `Arrow (at', d, v, c) -> `Arrow (at', ground d, v, ground c)
     | `Unk (at', _) -> `Star at'

let rec occurs_check at' v = function
  | `Star _ -> unit
  | `Arrow (_, d, _, c) -> occurs_check at' v d >> occurs_check at' v c
  | `Unk (_, v') ->
    if Unk.equal v v' then fail @@ `Error_cyclic_kind at' else unit

let rec unify at' lhs rhs =
  match (lhs, rhs) with
  | `Star _, `Star _ -> unit
  | `Arrow (_, ld, lv, lc), `Arrow (_, rd, rv, rc) (*when lv = rv*) ->
    unify at' ld rd
    >> let* lc = resolve lc and* rc = resolve rc in
       unify at' lc rc
  | `Unk (_, l), `Unk (_, r) when Unk.equal l r -> unit
  | `Unk (_, v), t | t, `Unk (_, v) -> occurs_check at' v t >> UnkMap.add v t
  | _ -> fail @@ `Error_kind_mismatch (at', lhs, rhs)

let unify at' lhs rhs =
  let* lhs = resolve lhs and* rhs = resolve rhs in
  unify at' lhs rhs
