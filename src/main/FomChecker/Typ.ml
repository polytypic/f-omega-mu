open FomSource
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

module Core = struct
  include Core

  let rec compare_in_env lhs_env rhs_env (l : Core.t) (r : Core.t) =
    compare' compare_in_env lhs_env rhs_env
      (l :> (_, _) FomAST.Typ.f)
      (r :> (_, _) FomAST.Typ.f)

  let compare l r = compare_in_env VarMap.empty VarMap.empty l r

  (* *)

  let unfold at f mu xs = apps_of_norm at (app_of_norm at f mu) xs

  let rec unfold_of_norm typ =
    match unapp typ with
    | (`Mu (at', f) as mu), xs -> unfold_of_norm (unfold at' f mu xs)
    | _ -> typ

  (* *)

  let rec ground t =
    t
    |> keep_phys_eq @@ function
       | `Lam (at', i, k, t) -> `Lam (at', i, Kind.ground k, ground t)
       | t -> map_eq ground t

  (* *)

  let rec resolve t =
    let+ t' =
      match t with
      | `Lam (at', d, k, t) ->
        Kind.resolve k <*> resolve t >>- fun (k, t) -> `Lam (at', d, k, t)
      | t -> map_eq_fr resolve t
    in
    keep_phys_eq' t t'
end

module GoalSet = Set.Make (struct
  type nonrec t = Var.t VarMap.t * Var.t VarMap.t * Core.t * Core.t

  let compare (l_env1, r_env1, l1, r1) (l_env2, r_env2, l2, r2) =
    Core.compare_in_env l_env1 l_env2 l1 l2 <>? fun () ->
    Core.compare_in_env r_env1 r_env2 r1 r2
end)

let make_sub_and_eq at =
  let goals = ref GoalSet.empty in
  let rec sub l_env r_env (l : Core.t) (r : Core.t) =
    let g = (l_env, r_env, l, r) in
    if 0 <> Core.compare_in_env l_env r_env l r && not (GoalSet.mem g !goals)
    then (
      goals := GoalSet.add g !goals;
      let rec subset l r flip ls ms =
        match (ls, ms) with
        | [], _ -> unit
        | (ll, _) :: _, [] -> fail @@ `Error_label_missing (at, ll, l, r)
        | ((ll, lt) :: ls as lls), (ml, mt) :: ms ->
          let c = Label.compare ll ml in
          if c = 0 then
            flip (sub l_env r_env) mt lt >> subset l r flip ls ms
          else if 0 < c then
            subset l r flip lls ms
          else
            fail @@ `Error_label_missing (at, ll, l, r)
      in
      match (l, r) with
      | `Arrow (_, ld, lc), `Arrow (_, rd, rc) ->
        sub r_env l_env rd ld >> sub l_env r_env lc rc
      | `Product (_, lls), `Product (_, rls) -> subset r l id rls lls
      | `Sum (_, lls), `Sum (_, rls) -> subset l r Fun.flip lls rls
      | `ForAll (_, l), `ForAll (_, r) | `Exists (_, l), `Exists (_, r) ->
        sub l_env r_env l r
      | `Lam (_, li, lk, lt), `Lam (_, ri, rk, rt) ->
        let v, l_env, r_env =
          if Var.equal li ri then
            (li, l_env |> VarMap.remove li, r_env |> VarMap.remove ri)
          else
            let v = Var.fresh Loc.dummy in
            (v, l_env |> VarMap.add li v, r_env |> VarMap.add ri v)
        in
        Kind.unify at lk rk >> sub l_env r_env lt rt
        |> VarMap.adding v @@ `Kind lk
      | _ -> (
        match (unapp l, unapp r) with
        | ((`Mu (la, lf) as lmu), lxs), _ ->
          sub l_env r_env (Core.unfold la lf lmu lxs) r
        | _, ((`Mu (ra, rf) as rmu), rxs) ->
          sub l_env r_env l (Core.unfold ra rf rmu rxs)
        | (`Var (_, lf), (_ :: _ as lxs)), (`Var (_, rf), (_ :: _ as rxs))
          when Var.equal lf rf && List.length lxs = List.length rxs -> (
          let* k_opt = VarMap.find_opt lf in
          match k_opt with
          | Some (`Kind _) -> List.iter2_fr (eq l_env r_env) lxs rxs
          | _ -> fail @@ `Error_typ_var_unbound (at, lf))
        | _ -> fail @@ `Error_typ_mismatch (at, (r :> t), (l :> t))))
    else
      unit
  and eq l_env r_env l r = sub l_env r_env l r >> sub r_env l_env r l in
  (sub, eq)

let check_sub_of_norm at = fst (make_sub_and_eq at) VarMap.empty VarMap.empty
let check_equal_of_norm at = snd (make_sub_and_eq at) VarMap.empty VarMap.empty

let as_predicate check l r =
  check Loc.dummy l r
  |> try_in (const @@ return true) (const @@ return false)
  |> Kind.UnkMap.cloning

let is_sub_of_norm l r = as_predicate check_sub_of_norm l r
let is_equal_of_norm l r = as_predicate check_equal_of_norm l r

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
  | `Join (_, l, _) | `Meet (_, l, _) -> kind_of l

and kind_of_cod checked_typ =
  let+ f_kind = kind_of checked_typ >>= Kind.resolve in
  match f_kind with
  | `Star _ | `Unk (_, _) -> failwith "kind_of_cod"
  | `Arrow (_, _, c_kind) -> c_kind

let kind_of t = kind_of t >>= Kind.resolve

(* *)

module Var = struct
  include Var

  let rec fresh_from = function
    | `Join (_, ((`Mu _ | `Lam _) as t), _)
    | `Join (_, _, ((`Mu _ | `Lam _) as t))
    | `Meet (_, _, ((`Mu _ | `Lam _) as t))
    | `Meet (_, ((`Mu _ | `Lam _) as t), _)
    | `Mu (_, t) ->
      fresh_from t
    | `Lam (_, i, _, t) -> (Var.freshen i, t)
    | t -> (Var.fresh (FomAST.Typ.at t), t)

  let rec fresh_from_n n t =
    if n <= 0 then
      []
    else
      fresh_from t |> fun (i, t) -> i :: fresh_from_n (n - 1) t
end

(* *)

module Solved = struct
  include Map.Make (FomAST.Typ)

  type nonrec t = FomAST.Typ.t t

  let field r : (t, _) Field.t = r#typ_solved

  class con =
    object
      val typ_solved : t = empty
      method typ_solved = Field.make typ_solved (fun v -> {<typ_solved = v>})
    end
end

(* *)

let union op (ls, rs) =
  Row.union_fr (const return) (const return) (const op) ls rs

let intersection op =
  let rec loop os = function
    | ((ll, lt) :: lls as llls), ((rl, rt) :: rls as rlls) ->
      let c = Label.compare ll rl in
      if c < 0 then
        loop os (lls, rlls)
      else if 0 < c then
        loop os (llls, rls)
      else
        op lt rt >>= fun t -> loop ((ll, t) :: os) (lls, rls)
    | [], _ | _, [] -> return @@ List.rev os
  in
  loop []

let rec mu_of_norm at = function
  | `Lam (_, i, _, t) when not (is_free i t) -> return t
  | `Lam (_, i, _, `Mu (at1, `Lam (at2, j, k, t))) ->
    let+ t = subst_of_norm (VarMap.singleton i @@ var j) t in
    `Mu (at1, `Lam (at2, j, k, t))
  | `Lam (at', i, k, t) as f ->
    let t', iks = unlam t in
    let* f' =
      match t' with
      | `Join _ | `Meet _ ->
        let mu' =
          List.fold_right (fun (i, _) f -> `App (at, f, var i)) iks (var i)
        in
        let+ t' = drop_legs mu' t' in
        let t' = List.fold_left (fun t (i, k) -> `Lam (at, i, k, t)) t' iks in
        `Lam (at', i, k, t')
      | _ -> return f
    in
    if compare f f' = 0 then
      return @@ `Mu (at, f)
    else
      mu_of_norm at f'
  | f -> return @@ `Mu (at, f)

and drop_legs x = function
  | `Join (at', l, r) ->
    let* r = drop_legs x r in
    if compare x l = 0 then
      return r
    else if compare x r = 0 then
      return l
    else
      join_of_norm at' l r
  | `Meet (at', l, r) ->
    let* r = drop_legs x r in
    if compare x l = 0 then
      return r
    else if compare x r = 0 then
      return l
    else
      meet_of_norm at' l r
  | t -> return t

and lam_of_norm at i k = function
  | `App (_, f, `Var (_, i')) when Var.equal i i' && not (is_free i f) -> f
  | t' -> `Lam (at, i, k, t')

and app_of_norm at f' x' =
  match f' with
  | `Lam (_, i, _, t) -> subst_of_norm (VarMap.singleton i x') t
  | f' -> return @@ `App (at, f', x')

and apps_of_norm at' = List.fold_left_fr (app_of_norm at')

and unfold at f mu xs =
  app_of_norm at f mu >>= fun f_mu -> apps_of_norm at f_mu xs

and classify t =
  match unapp t with
  | (`Mu (at', f) as mu), xs ->
    let mu', _ = Var.fresh_from f in
    let* k = kind_of mu in
    unfold at' f (var mu') xs >>= classify |> VarMap.adding mu' @@ `Kind k
  | `Var _, _ -> return None
  | `Const (_, c), _ -> return @@ Some (`Const c)
  | `Lam _, _ -> return @@ Some `Lam
  | `ForAll _, _ -> return @@ Some `ForAll
  | `Exists _, _ -> return @@ Some `Exists
  | `Arrow _, _ -> return @@ Some `Arrow
  | `Product _, _ -> return @@ Some `Product
  | `Sum _, _ -> return @@ Some `Sum
  | `Join (_, l, r), _ -> (
    classify l >>= function None -> classify r | some -> return some)
  | `Meet (_, l, r), _ -> (
    classify l >>= function None -> classify r | some -> return some)
  | `App _, _ -> failwith "classify"

and memoing t op =
  let* m = get Solved.field in
  match Solved.find_opt t m with
  | Some t -> return t
  | None ->
    let at' = at t in
    let i, _ = Var.fresh_from t in
    let v = var i in
    let* t' = op |> mapping Solved.field (Solved.add t v) in
    kind_of t >>= fun k -> mu_of_norm at' (lam_of_norm at' i k t')

and join_or_meet_of_norm con lower upper sum product at' l r =
  if compare l r = 0 then
    return l
  else
    match (l, r) with
    | `Arrow (_, ld, lc), `Arrow (_, rd, rc) ->
      lower ld rd <*> upper lc rc >>- fun (d, c) -> `Arrow (at', d, c)
    | `Product (_, lls), `Product (_, rls) ->
      product upper (lls, rls) >>- fun ls -> `Product (at', ls)
    | `Sum (_, lls), `Sum (_, rls) ->
      sum upper (lls, rls) >>- fun ls -> `Sum (at', ls)
    | `Lam (_, li, lk, lt), `Lam (_, ri, rk, rt) ->
      let* i, lt, rt =
        if Var.equal li ri then
          return (li, lt, rt)
        else if not (is_free li rt) then
          subst_of_norm (VarMap.singleton ri (var li)) rt >>- fun rt ->
          (li, lt, rt)
        else if not (is_free ri lt) then
          subst_of_norm (VarMap.singleton li (var ri)) lt >>- fun lt ->
          (ri, lt, rt)
        else
          let i = Var.freshen li in
          let v = var i in
          let+ lt = subst_of_norm (VarMap.singleton li v) lt
          and+ rt = subst_of_norm (VarMap.singleton ri v) rt in
          (i, lt, rt)
      in
      Kind.unify at' lk rk >> upper lt rt |> VarMap.adding i @@ `Kind lk
      >>- fun t -> `Lam (at', i, lk, t)
    | `ForAll (_, lt), `ForAll (_, rt) ->
      upper lt rt >>- fun t -> `ForAll (at', t)
    | `Exists (_, lt), `Exists (_, rt) ->
      upper lt rt >>- fun t -> `Exists (at', t)
    | _ -> (
      let problem = con (at', l, r) in
      match (unapp l, unapp r) with
      | ((`Mu (la, lf) as lmu), lxs), _ ->
        unfold la lf lmu lxs >>= fun l -> upper l r |> memoing problem
      | _, ((`Mu (ra, rf) as rmu), rxs) ->
        unfold ra rf rmu rxs >>= fun r -> upper l r |> memoing problem
      | _ -> (
        classify l <*> classify r >>= fun (l', r') ->
        match Option.both ( = ) l' r' with
        | Some false -> fail @@ `Error_typ_mismatch (at', l, r)
        | _ -> return problem))

and join_of_norm at' l r =
  join_or_meet_of_norm
    (function
      | at1, `Join (at2, a, b), c -> `Join (at1, a, `Join (at2, b, c))
      | x -> `Join x)
    (meet_of_norm at') (join_of_norm at') union intersection at' l r

and meet_of_norm at' l r =
  join_or_meet_of_norm
    (function
      | at1, `Meet (at2, a, b), c -> `Meet (at1, a, `Meet (at2, b, c))
      | x -> `Meet x)
    (join_of_norm at') (meet_of_norm at') intersection union at' l r

and subst_of_norm env t =
  let+ t' =
    match t with
    | `Var (_, i) as inn -> (
      match FomAST.Typ.VarMap.find_opt i env with
      | None -> return inn
      | Some t -> return t)
    | `Mu (at, t) -> subst_of_norm env t >>= mu_of_norm at
    | `Lam (at, i, k, t) as inn ->
      let env = VarMap.remove i env in
      if VarMap.is_empty env then
        return inn
      else if VarMap.exists (fun i' t' -> is_free i t' && is_free i' t) env then
        let i' = Var.freshen i in
        let v' = `Var (at, i') in
        let+ t' = subst_of_norm (VarMap.add i v' env) t in
        lam_of_norm at i' k t'
      else
        subst_of_norm env t |> VarMap.adding i @@ `Kind k >>- lam_of_norm at i k
    | `App (at, f, x) ->
      subst_of_norm env f <*> subst_of_norm env x >>= fun (f, x) ->
      app_of_norm at f x
    | `Join (at, l, r) ->
      subst_of_norm env l <*> subst_of_norm env r >>= fun (l, r) ->
      join_of_norm at l r
    | `Meet (at, l, r) ->
      subst_of_norm env l <*> subst_of_norm env r >>= fun (l, r) ->
      meet_of_norm at l r
    | t -> map_eq_fr (subst_of_norm env) t
  in
  keep_phys_eq' t t'

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
    return None
  else
    let i = Var.fresh at in
    let v = var i in
    let is = List.init arity @@ fun _ -> Var.fresh at in
    let vs = is |> List.map var in
    app_of_norm at f v >>= fun fv ->
    apps_of_norm at fv vs
    >>- find_map_from_all_apps_of i @@ fun _ _ xs ->
        xs
        |> List.find_map @@ function
           | `Var _ -> None
           | t ->
             is
             |> List.find_map @@ fun i -> if is_free i t then Some t else None

(* *)

let rec find_opt_non_contractive ids typ =
  match unapp typ with
  | `Mu (_, `Lam (_, id, _, f)), xs -> (
    apps_of_norm Loc.dummy f xs >>- unapp >>= function
    | (`Var (_, id') as mu), _ when Var.equal id' id || VarSet.mem id' ids ->
      return @@ Some mu
    | typ, _ -> find_opt_non_contractive (VarSet.add id ids) typ)
  | _ -> return None

let find_opt_non_contractive_mu at f arity =
  match f with
  | `Lam (_, id, _, f) -> (
    let xs = List.init arity @@ fun _ -> var @@ Var.fresh at in
    apps_of_norm Loc.dummy f xs >>- unapp >>= function
    | (`Var (_, id') as mu), _ when Var.equal id' id -> return @@ Some mu
    | typ, _ -> find_opt_non_contractive (VarSet.singleton id) typ)
  | _ -> return None

(* *)

let rec resolve t =
  let+ t' =
    match t with
    | `Lam (at', d, k, t) ->
      Kind.resolve k <*> resolve t >>- fun (k, t) -> `Lam (at', d, k, t)
    | t -> map_eq_fr resolve t
  in
  keep_phys_eq' t t'

(* *)

let rec infer = function
  | `Mu (at', f) as typ ->
    let* f, f_kind = infer f in
    let kind = Kind.fresh at' in
    Kind.unify at' (`Arrow (at', kind, kind)) f_kind
    >> let* arity = Kind.resolve kind >>- Kind.min_arity in
       find_opt_nested_arg_mu at' f arity
       >>= Option.iter_fr (fun typ' ->
               let* typ = resolve typ and* typ' = resolve typ' in
               fail @@ `Error_mu_nested (at', typ, typ'))
       >> (find_opt_non_contractive_mu at' f arity
          >>= Option.iter_fr (fun typ' ->
                  let* typ = resolve typ and* typ' = resolve typ' in
                  fail @@ `Error_mu_non_contractive (at', typ, typ')))
       >> (mu_of_norm at' f <*> return kind)
  | `Const (at', c) as t -> return @@ (t, Const.kind_of at' c)
  | `Var (at', i) as t -> (
    let* i_kind_opt = VarMap.find_opt i in
    match i_kind_opt with
    | Some (`Kind i_kind) -> return (t, i_kind)
    | _ -> fail @@ `Error_typ_var_unbound (at', i))
  | `Lam (at', d, d_kind, r) ->
    let+ r, r_kind = infer r |> VarMap.adding d @@ `Kind d_kind in
    (lam_of_norm at' d d_kind r, `Arrow (at', d_kind, r_kind))
  | `App (at', f, x) ->
    let* f, f_kind = infer f and* x, d_kind = infer x in
    let c_kind = Kind.fresh at' in
    Kind.unify at' (`Arrow (at', d_kind, c_kind)) f_kind
    >> (app_of_norm at' f x <*> return c_kind)
  | `ForAll (at', f) -> infer_quantifier at' f @@ fun at' f -> `ForAll (at', f)
  | `Exists (at', f) -> infer_quantifier at' f @@ fun at' f -> `Exists (at', f)
  | `Arrow (at', d, c) ->
    let star = `Star at' in
    check star d <*> check star c >>- fun (d, c) -> (`Arrow (at', d, c), star)
  | `Product (at', ls) -> infer_row at' ls @@ fun at' ls -> `Product (at', ls)
  | `Sum (at', ls) -> infer_row at' ls @@ fun at' ls -> `Sum (at', ls)
  | `Join (at', l, r) ->
    infer l <*> infer r >>= fun ((l, lk), (r, rk)) ->
    Kind.unify at' lk rk >> (join_of_norm at' l r <*> return lk)
  | `Meet (at', l, r) ->
    infer l <*> infer r >>= fun ((l, lk), (r, rk)) ->
    Kind.unify at' lk rk >> (meet_of_norm at' l r <*> return lk)

and infer_row at' ls con =
  let star = `Star at' in
  let+ ls = Row.check ls >> Row.map_fr (check star) ls in
  (con at' ls, star)

and infer_quantifier at' f con =
  let* f, f_kind = infer f in
  let d_kind = Kind.fresh at' and c_kind = `Star at' in
  Kind.unify at' (`Arrow (at', d_kind, c_kind)) f_kind
  >> return (con at' f, c_kind)

and check expected t =
  let* t, actual = infer t in
  Kind.unify (at t) expected actual >> return t

(* *)

let rec solve_of_norm = function
  | #Core.f as t -> Core.map_fr solve_of_norm t
  | `Join (at', l, r) | `Meet (at', l, r) ->
    fail @@ `Error_typ_mismatch (at', l, r)

let join_of_norm at' (l : Core.t) (r : Core.t) =
  join_of_norm at' (l :> t) (r :> t) >>= solve_of_norm

(* *)

let infer t = infer t >>= Pair.map_fr solve_of_norm return
let check k = check k >=> solve_of_norm

(* *)

let infer_and_resolve t = infer t >>= (fst >>> Core.resolve)
let check_and_resolve k = check k >=> Core.resolve
