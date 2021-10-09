open FomSource
open FomAnnot
open FomBasis

(* *)

include FomAST.Typ

(* *)

module VarMap = struct
  include VarMap

  let field r = r#typ_env
  let find_opt i = get_as field @@ find_opt i
  let existing pr = get_as field @@ exists pr
  let resetting_to initial op = setting field initial op
  let adding i v = mapping field @@ add i v
  let merging env = mapping field (merge Map.prefer_lhs env)

  class ['t] con =
    object
      val typ_env : 't t = empty
      method typ_env = Field.make typ_env (fun v -> {<typ_env = v>})
    end
end

(* *)

let rec find_map_from_all_apps_of i' p = function
  | `Lam (_, i, _, t) ->
    if Var.equal i i' then None else find_map_from_all_apps_of i' p t
  | (`App _ | `Var _) as t -> (
    match unapp t with
    | (`Var (_, i) as f), xs ->
      if Var.equal i i' then
        match p t f xs with
        | None -> xs |> List.find_map (find_map_from_all_apps_of i' p)
        | some -> some
      else
        xs |> List.find_map (find_map_from_all_apps_of i' p)
    | f, xs -> (
      match find_map_from_all_apps_of i' p f with
      | None -> xs |> List.find_map (find_map_from_all_apps_of i' p)
      | some -> some))
  | t -> find_map (find_map_from_all_apps_of i' p) t

let find_opt_nested_arg_mu at f arity =
  if arity <= 0 then
    None
  else
    let i = Var.fresh at in
    let v = var i in
    let is = List.init arity @@ fun _ -> Var.fresh at in
    let vs = is |> List.map var in
    apps_of_norm at (app_of_norm at f v) vs
    |> find_map_from_all_apps_of i @@ fun _ _ xs ->
       xs
       |> List.find_map @@ function
          | `Var _ -> None
          | t ->
            is |> List.find_map @@ fun i -> if is_free i t then Some t else None

(* *)

let rec find_opt_non_contractive ids typ =
  match unapp typ with
  | `Mu (_, `Lam (_, id, _, f)), xs -> (
    match apps_of_norm Loc.dummy f xs |> unapp with
    | (`Var (_, id') as mu), _ when Var.equal id' id || VarSet.mem id' ids ->
      Some mu
    | typ, _ -> find_opt_non_contractive (VarSet.add id ids) typ)
  | _ -> None

let find_opt_non_contractive_mu at f arity =
  match f with
  | `Lam (_, id, _, f) -> (
    let is = List.init arity @@ fun _ -> Var.fresh at in
    let xs = is |> List.map var in
    match apps_of_norm Loc.dummy f xs |> unapp with
    | (`Var (_, id') as mu), _ when Var.equal id' id -> Some mu
    | typ, _ -> find_opt_non_contractive (VarSet.singleton id) typ)
  | _ -> None

(* *)

let rec resolve t =
  let+ t' =
    match t with
    | `Lam (at', d, d_kind, r) ->
      let+ d_kind' = Kind.resolve d_kind and+ r' = resolve r in
      `Lam (at', d, d_kind', r')
    | t -> map_eq_fr resolve t
  in
  keep_phys_eq' t t'

let resolve_counter = Profiling.Counter.register "resolve"

let resolve t =
  Profiling.Counter.inc resolve_counter;
  resolve t

(* *)

let rec ground t =
  t
  |> keep_phys_eq @@ function
     | `Lam (at', d, d_kind, r) -> `Lam (at', d, Kind.ground d_kind, ground r)
     | t -> map_eq ground t

let ground = Profiling.Counter.wrap'1 "ground" ground

(* *)

let rec infer = function
  | `Mu (at', f) as typ ->
    let* f, f_kind = infer f in
    let kind = Kind.fresh at' in
    Kind.unify at' (`Arrow (at', kind, kind)) f_kind
    >> let* arity = Kind.resolve kind >>- Kind.min_arity in
       find_opt_nested_arg_mu at' f arity
       |> Option.iter_fr (fun typ' ->
              let* typ = resolve typ and* typ' = resolve typ' in
              fail @@ `Error_mu_nested (at', typ, typ'))
       >> (find_opt_non_contractive_mu at' f arity
          |> Option.iter_fr (fun typ' ->
                 let* typ = resolve typ and* typ' = resolve typ' in
                 fail @@ `Error_mu_non_contractive (at', typ, typ')))
       >> return (mu_of_norm at' f, kind)
  | `Const (at', c) as t -> return @@ (t, Const.kind_of at' c)
  | `Var (at', i) as t -> (
    let* i_kind_opt = VarMap.find_opt i in
    match i_kind_opt with
    | Some (`Kind i_kind) ->
      Annot.Typ.use i (Kind.at i_kind) >> return (t, i_kind)
    | _ -> fail @@ `Error_typ_var_unbound (at', i))
  | `Lam (at', d, d_kind, r) ->
    let+ r, r_kind =
      Annot.Typ.def d d_kind >> infer r |> VarMap.adding d @@ `Kind d_kind
    in
    (lam_of_norm at' d d_kind r, `Arrow (at', d_kind, r_kind))
  | `App (at', f, x) ->
    let* f, f_kind = infer f and* x, d_kind = infer x in
    let c_kind = Kind.fresh at' in
    Kind.unify at' (`Arrow (at', d_kind, c_kind)) f_kind
    >> return (app_of_norm at' f x, c_kind)
  | `ForAll (at', f) -> infer_quantifier at' f @@ fun at' f -> `ForAll (at', f)
  | `Exists (at', f) -> infer_quantifier at' f @@ fun at' f -> `Exists (at', f)
  | `Arrow (at', d, c) ->
    let star = `Star at' in
    let+ d = check star d and+ c = check star c in
    (`Arrow (at', d, c), star)
  | `Product (at', ls) -> infer_row at' ls @@ fun at' ls -> `Product (at', ls)
  | `Sum (at', ls) -> infer_row at' ls @@ fun at' ls -> `Sum (at', ls)

and infer_row at' ls con =
  let star = `Star at' in
  let+ ls = Row.check ls >> Row.map_fr (check star) ls in
  (con at' ls, star)

and infer_quantifier at' f con =
  let* f, f_kind = infer f in
  let d_kind = Kind.fresh at' in
  let c_kind = `Star at' in
  Kind.unify at' (`Arrow (at', d_kind, c_kind)) f_kind
  >> return (con at' f, c_kind)

and check expected t =
  let* t, actual = infer t in
  Kind.unify (at t) expected actual >> return t

(* *)

let infer_and_resolve t = infer t >>= fun (t, _) -> resolve t

(* *)

let rec kind_of = function
  | `Mu (_, f) -> kind_of_cod f
  | `Const (at', c) -> return @@ Const.kind_of at' c
  | `Var (_, i) -> (
    let+ i_kind_opt = VarMap.find_opt i in
    match i_kind_opt with
    | Some (`Kind i_kind) -> i_kind
    | _ -> failwithf "kind_of %s" @@ Var.to_string i)
  | `Lam (at', d, d_kind, r) ->
    let+ r_kind = kind_of r |> VarMap.adding d @@ `Kind d_kind in
    `Arrow (at', d_kind, r_kind)
  | `App (_, f, _) -> kind_of_cod f
  | `ForAll (at', _)
  | `Exists (at', _)
  | `Arrow (at', _, _)
  | `Product (at', _)
  | `Sum (at', _) ->
    return @@ `Star at'

and kind_of_cod checked_typ =
  let+ f_kind = kind_of checked_typ >>= Kind.resolve in
  match f_kind with
  | `Star _ | `Unk (_, _) -> failwith "kind_of_cod"
  | `Arrow (_, _, c_kind) -> c_kind

let kind_of t = kind_of t >>= Kind.resolve

(* *)

let unfold at f mu xs = apps_of_norm at (app_of_norm at f mu) xs

let rec unfold_of_norm typ =
  match unapp typ with
  | (`Mu (at', f) as mu), xs -> unfold_of_norm (unfold at' f mu xs)
  | _ -> typ

(* *)

module Rename = FomAST.Typ.VarMap

module GoalSet = Set.Make (struct
  type nonrec t = Var.t Rename.t * Var.t Rename.t * t * t

  let compare (l_env1, r_env1, l1, r1) (l_env2, r_env2, l2, r2) =
    compare_in_env l_env1 l_env2 l1 l2 <>? fun () ->
    compare_in_env r_env1 r_env2 r1 r2
end)

let check_sub_of_norm, check_equal_of_norm =
  let make_sub_and_eq at =
    let goals = ref GoalSet.empty in
    let rec sub l_env r_env l r =
      let g = (l_env, r_env, l, r) in
      if 0 <> compare_in_env l_env r_env l r && not (GoalSet.mem g !goals) then (
        goals := GoalSet.add g !goals;
        let rec subset swap ls ms =
          match (ls, ms) with
          | [], _ -> unit
          | (ll, _) :: _, [] -> fail @@ `Error_label_missing (at, ll, l, r)
          | ((ll, lt) :: ls as lls), (ml, mt) :: ms ->
            let c = Label.compare ll ml in
            if c = 0 then
              (if swap then sub l_env r_env lt mt else sub l_env r_env mt lt)
              >> subset swap ls ms
            else if 0 < c then
              subset swap lls ms
            else
              fail @@ `Error_label_missing (at, ll, l, r)
        in
        match (l, r) with
        | `Arrow (_, ld, lc), `Arrow (_, rd, rc) ->
          sub r_env l_env rd ld >> sub l_env r_env lc rc
        | `Product (_, lls), `Product (_, rls) -> subset false rls lls
        | `Sum (_, lls), `Sum (_, rls) -> subset true lls rls
        | `ForAll (_, l), `ForAll (_, r) | `Exists (_, l), `Exists (_, r) ->
          sub l_env r_env l r
        | `Lam (_, li, lk, lt), `Lam (_, ri, rk, rt) ->
          let l_env, r_env =
            if Var.equal li ri then
              (l_env |> Rename.remove li, r_env |> Rename.remove ri)
            else
              let v = Var.fresh Loc.dummy in
              (l_env |> Rename.add li v, r_env |> Rename.add ri v)
          in
          Kind.unify at lk rk >> sub l_env r_env lt rt
        | _ -> (
          match (unapp l, unapp r) with
          | ((`Mu (la, lf) as lmu), lxs), ((`Mu (ra, rf) as rmu), rxs) ->
            sub l_env r_env (unfold la lf lmu lxs) (unfold ra rf rmu rxs)
          | ((`Mu (la, lf) as lmu), lxs), _ ->
            sub l_env r_env (unfold la lf lmu lxs) r
          | _, ((`Mu (ra, rf) as rmu), rxs) ->
            sub l_env r_env l (unfold ra rf rmu rxs)
          | (lf, lx :: lxs), (rf, rx :: rxs)
            when List.length lxs = List.length rxs ->
            eq l_env r_env lf rf >> eq l_env r_env lx rx
            >> List.iter2_fr (eq l_env r_env) lxs rxs
          | _ -> fail @@ `Error_typ_mismatch (at, r, l)))
      else
        unit
    and eq l_env r_env l r = sub l_env r_env l r >> sub r_env l_env r l in
    (sub, eq)
  in
  let sub at l r = fst (make_sub_and_eq at) Rename.empty Rename.empty l r in
  let eq at l r = snd (make_sub_and_eq at) Rename.empty Rename.empty l r in
  (sub, eq)

let is_sub_of_norm, is_equal_of_norm =
  let as_predicate check l r =
    check Loc.dummy l r |> try_in (const @@ return true) (const @@ return false)
  in
  let sub l r = as_predicate check_sub_of_norm l r in
  let eq l r = as_predicate check_equal_of_norm l r in
  (sub, eq)

(* *)

module TypSet = Set.Make (FomAST.Typ)

let rec contract t =
  let* s, t = contract_base t in
  let+ t_opt = s |> TypSet.elements |> List.find_opt_fr (is_equal_of_norm t) in
  let s, u =
    match t_opt with
    | Some t -> (s, t)
    | None -> (
      match unapp t with `Mu _, _ -> (TypSet.add t s, t) | _ -> (s, t))
  in
  match t with
  | `Lam (_, i, _, _) -> (s |> TypSet.filter (not <<< is_free i), u)
  | _ -> (s, u)

and contract_base t =
  let+ s, t' =
    match t with
    | `Mu (at', e) -> contract e >>- fun (s, e') -> (s, `Mu (at', e'))
    | (`Const (_, _) | `Var (_, _)) as t -> return (TypSet.empty, t)
    | `Lam (at', x, k, e) ->
      contract e >>- fun (s, e') -> (s, `Lam (at', x, k, e'))
    | `App (at', f, x) ->
      let+ fs, f' = contract f and+ xs, x' = contract x in
      (TypSet.union fs xs, `App (at', f', x'))
    | `ForAll (at', e) -> contract e >>- fun (s, e') -> (s, `ForAll (at', e'))
    | `Exists (at', e) -> contract e >>- fun (s, e') -> (s, `Exists (at', e'))
    | `Arrow (at', d, c) ->
      let+ ds, d' = contract d and+ cs, c' = contract c in
      (TypSet.union ds cs, `Arrow (at', d', c'))
    | `Product (at', ls) ->
      contract_labels ls >>- fun (s, ls') -> (s, `Product (at', ls'))
    | `Sum (at', ls) ->
      contract_labels ls >>- fun (s, ls') -> (s, `Sum (at', ls'))
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
  | `App (_, f, x) ->
    let mus, f_vs = collect_mus_closed bvs f mus in
    let mus, x_vs = collect_mus_closed bvs x mus in
    (mus, VarSet.union f_vs x_vs)
  | `Lam (_, i, _, e) ->
    let mus, vs = collect_mus_closed (VarSet.add i bvs) e mus in
    (mus, VarSet.remove i vs)
  | `ForAll (_, e) | `Exists (_, e) -> collect_mus_closed bvs e mus
  | `Arrow (_, d, c) ->
    let mus, d_vs = collect_mus_closed bvs d mus in
    let mus, c_vs = collect_mus_closed bvs c mus in
    (mus, VarSet.union d_vs c_vs)
  | `Product (_, ls) | `Sum (_, ls) ->
    ls
    |> List.fold_left
         (fun (mus, vs) (_, t) ->
           let mus, t_vs = collect_mus_closed bvs t mus in
           (mus, VarSet.union vs t_vs))
         (mus, VarSet.empty)

let rec replace_closed_mus m =
  keep_phys_eq @@ function
  | `Mu (at'', `Lam (at', i, k, e)) as t ->
    if TypSet.mem t m then
      `Var (at', i)
    else
      `Mu (at'', `Lam (at', i, k, replace_closed_mus m e))
  | t -> map_eq (replace_closed_mus m) t

(* *)

let to_strict t =
  let bound = ref [] in
  let rec to_strict (t : [('a, 'k) f | `Lazy of ('e, 'a) LVar.t] as 'a) =
    match t with
    | #f as t -> map_fr to_strict t
    | `Lazy t -> (
      match !bound |> List.find_opt (fun (t', _, _) -> t == t') with
      | None ->
        let n = ref 0 in
        let id = Var.fresh Loc.dummy in
        bound := (t, id, n) :: !bound;
        let* t = LVar.get t >>= to_strict in
        bound := List.tl !bound;
        if !n <> 0 then
          let+ k = kind_of t in
          `Mu (Loc.dummy, `Lam (Loc.dummy, id, k, t))
        else
          return t
      | Some (_, id, n) ->
        n := !n + 1;
        return @@ var id)
  in
  to_strict t

let to_lazy e =
  (e : ('a, 'k) f as 'a :> [('b, 'k) f | `Lazy of ('e, 'b) LVar.t] as 'b)

module GoalMap = Map.Make (Compare.Tuple'2 (FomAST.Typ) (FomAST.Typ))

let join_of_norm, meet_of_norm =
  let make_join_and_meet at =
    let joins = ref GoalMap.empty in
    let meets = ref GoalMap.empty in
    let memo_in map g op =
      match GoalMap.find_opt g !map with
      | Some result -> return result
      | None ->
        let+ var = LVar.create (op g) in
        let result = `Lazy var in
        map := GoalMap.add g result !map;
        result
    in
    let rec intersection op os = function
      | ((ll, lt) :: lls as llls), ((rl, rt) :: rls as rlls) ->
        let c = Label.compare ll rl in
        if c < 0 then
          intersection op os (lls, rlls)
        else if 0 < c then
          intersection op os (llls, rls)
        else
          op lt rt >>= fun t -> intersection op ((ll, t) :: os) (lls, rls)
      | [], _ | _, [] -> return @@ List.rev os
    in
    let rec union op os = function
      | ((ll, lt) :: lls as llls), ((rl, rt) :: rls as rlls) ->
        let c = Label.compare ll rl in
        if c < 0 then
          union op ((ll, to_lazy lt) :: os) (lls, rlls)
        else if 0 < c then
          union op ((rl, to_lazy rt) :: os) (llls, rls)
        else
          op lt rt >>= fun t -> union op ((ll, t) :: os) (lls, rls)
      | (ll, lt) :: lls, [] -> union op ((ll, to_lazy lt) :: os) (lls, [])
      | [], (rl, rt) :: rls -> union op ((rl, to_lazy rt) :: os) ([], rls)
      | [], [] -> return @@ List.rev os
    in
    let synth map fst snd upper lower intersection union l r =
      memo_in map (l, r) @@ fun g ->
      let* g_is_sub = is_sub_of_norm l r in
      if g_is_sub then
        return @@ to_lazy (snd g)
      else
        let* swap_g_is_sub = is_sub_of_norm r l in
        if swap_g_is_sub then
          return @@ to_lazy (fst g)
        else
          match g with
          | `Arrow (_, ld, lc), `Arrow (_, rd, rc) ->
            lower ld rd <*> upper lc rc >>- fun (d, c) -> `Arrow (at, d, c)
          | `Product (_, lls), `Product (_, rls) ->
            intersection upper [] (lls, rls) >>- fun ls -> `Product (at, ls)
          | `Sum (_, lls), `Sum (_, rls) ->
            union upper [] (lls, rls) >>- fun ls -> `Sum (at, ls)
          | `Lam (_, li, lk, lt), `Lam (_, ri, rk, rt) ->
            let i, lt, rt =
              if Var.equal li ri then
                (li, lt, rt)
              else if not (is_free li rt) then
                (li, lt, subst_of_norm (VarMap.singleton ri (var li)) rt)
              else if not (is_free ri lt) then
                (ri, subst_of_norm (VarMap.singleton li (var ri)) lt, rt)
              else
                let i = Var.fresh at in
                ( i,
                  subst_of_norm (VarMap.singleton li (var i)) lt,
                  subst_of_norm (VarMap.singleton ri (var i)) rt )
            in
            Kind.unify at lk rk >> upper lt rt >>- fun t -> `Lam (at, i, lk, t)
          | `ForAll (_, lt), `ForAll (_, rt) ->
            upper lt rt >>- fun t -> `ForAll (at, t)
          | `Exists (_, lt), `Exists (_, rt) ->
            upper lt rt >>- fun t -> `Exists (at, t)
          | _ -> (
            match (unapp l, unapp r) with
            | ((`Mu (la, lf) as lmu), lxs), ((`Mu (ra, rf) as rmu), rxs) ->
              upper (unfold la lf lmu lxs) (unfold ra rf rmu rxs)
            | ((`Mu (la, lf) as lmu), lxs), _ -> upper (unfold la lf lmu lxs) r
            | _, ((`Mu (ra, rf) as rmu), rxs) -> upper l (unfold ra rf rmu rxs)
            | _ -> fail @@ `Error_typ_mismatch (at, l, r))
    in
    let rec join l r = synth joins fst snd join meet intersection union l r
    and meet l r = synth meets snd fst meet join union intersection l r in
    (join, meet)
  in
  let join at l r = fst (make_join_and_meet at) l r >>= to_strict
  and meet at l r = snd (make_join_and_meet at) l r >>= to_strict in
  (join, meet)
