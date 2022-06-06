open FomBasis
open FomChecker

(* *)

include FomPP.Typ
include FomChecker.Typ
include FomChecker.Typ.Core

(* Transforms to make types more legible - Not needed for type checking *)

module TypSet = Set.Make (Core)

let rec contract t =
  let* s, t = contract_base t in
  let* t_opt = s |> TypSet.elements |> List.find_opt_fr (is_equal_of_norm t) in
  let s, u =
    match t_opt with
    | Some t -> (s, t)
    | None -> (
      match unapp t with `Mu _, _ -> (TypSet.add t s, t) | _ -> (s, t))
  in
  match t with
  | `Lam (_, i, _, _) -> TypSet.filter_fr (is_free i >-> not) s <*> return u
  | _ -> return (s, u)

and contract_base t =
  let+ s, t' =
    match t with
    | `Mu (at', e) -> contract e >>- fun (s, e') -> (s, `Mu (at', e'))
    | (`Const _ | `Var _ | `Unk _) as t -> return (TypSet.empty, t)
    | `Lam (at', x, k, e) ->
      contract e >>- fun (s, e') -> (s, `Lam (at', x, k, e'))
    | `App (at', f, x) ->
      let+ fs, f' = contract f and+ xs, x' = contract x in
      (TypSet.union fs xs, `App (at', f', x'))
    | `Arrow (at', d, c) ->
      let+ ds, d' = contract d and+ cs, c' = contract c in
      (TypSet.union ds cs, `Arrow (at', d', c'))
    | `For (at', q, e) -> contract e >>- fun (s, e') -> (s, `For (at', q, e'))
    | `Row (at', m, ls) ->
      contract_labels ls >>- fun (s, ls') -> (s, `Row (at', m, ls'))
  in
  (s, keep_phys_eq' t t')

and contract_labels ls =
  let+ sls' = ls |> Row.map_fr contract in
  let ls' =
    sls' |> Row.map snd
    |> List.share_phys_eq (Pair.share_phys_eq (fun _ x -> x) (fun _ x -> x)) ls
  in
  let s =
    sls'
    |> List.fold_left (fun s (_, (s', _)) -> TypSet.union s s') TypSet.empty
  in
  (s, ls')

let contract t = contract t >>- snd

(* *)

let rec collect_mus_closed bvs t mus =
  match t with
  | `Mu (_, `Lam (_, i, _, e)) as t ->
    let mus, e_vs = collect_mus_closed (VarSet.add i bvs) e mus in
    let e_vs = VarSet.remove i e_vs in
    let mus = if VarSet.disjoint bvs e_vs then TypSet.add t mus else mus in
    (mus, e_vs)
  | `Mu (_, e) -> collect_mus_closed bvs e mus
  | `Const _ -> (mus, VarSet.empty)
  | `Var (_, i) -> (mus, VarSet.singleton i)
  | `Unk _ -> (mus, VarSet.empty)
  | `App (_, f, x) ->
    let mus, f_vs = collect_mus_closed bvs f mus in
    let mus, x_vs = collect_mus_closed bvs x mus in
    (mus, VarSet.union f_vs x_vs)
  | `Lam (_, i, _, e) ->
    let mus, vs = collect_mus_closed (VarSet.add i bvs) e mus in
    (mus, VarSet.remove i vs)
  | `For (_, _, e) -> collect_mus_closed bvs e mus
  | `Arrow (_, d, c) ->
    let mus, d_vs = collect_mus_closed bvs d mus in
    let mus, c_vs = collect_mus_closed bvs c mus in
    (mus, VarSet.union d_vs c_vs)
  | `Row (_, _, ls) ->
    ls
    |> List.fold_left
         (fun (mus, vs) (_, t) ->
           let mus, t_vs = collect_mus_closed bvs t mus in
           (mus, VarSet.union vs t_vs))
         (mus, VarSet.empty)

let rec replace_closed_mus m =
  keep_phys_eq @@ function
  | `Mu (at'', `Lam (at', i, k, e)) as t ->
    if TypSet.mem t m then `Var (at', i)
    else `Mu (at'', `Lam (at', i, k, replace_closed_mus m e))
  | t -> map_eq (replace_closed_mus m) t
