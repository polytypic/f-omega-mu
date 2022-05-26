open FomSource
open FomBasis

(* *)

include FomAST.Typ

(* *)

module VarEnv = struct
  type ('t, 'r) m = ('t VarMap.t, 'r) Field.t
  type ('t, 'r) f = < typ_env : ('t, 'r) m >

  let field r = r#typ_env

  let find i =
    get_as field @@ VarMap.find_opt i >>= function
    | Some t -> return t
    | None -> fail @@ `Error_typ_var_unbound (Var.at i, i)

  let kind_of i =
    get_as field @@ VarMap.find_opt i >>= function
    | Some (`Kind k) -> return k
    | _ -> fail @@ `Error_typ_var_unbound (Var.at i, i)

  let existing pr = get_as field @@ VarMap.exists pr
  let existing_fr pr = get field >>= VarMap.exists_fr pr
  let resetting_to initial op = setting field initial op
  let adding i v = mapping field @@ VarMap.add i v
  let merging env = mapping field (VarMap.merge Map.prefer_lhs env)

  class ['t] con =
    object
      val typ_env = VarMap.empty
      method typ_env : ('t, _) m = Field.make typ_env (fun v -> {<typ_env = v>})
    end
end

(* *)

let rec kind_of = function
  | `Mu (_, f) -> kind_of_cod f
  | `Const (at', c) -> return @@ Const.kind_of at' c
  | `Var (_, i) -> VarEnv.kind_of i
  | `Lam (at', i, d, t) ->
    kind_of t |> VarEnv.adding i @@ `Kind d >>- fun c -> `Arrow (at', d, c)
  | `App (_, f, _) -> kind_of_cod f
  | `Arrow (at', _, _) | `For (at', _, _) | `Row (at', _, _) ->
    return @@ `Star at'
  | `Bop (_, _, l, _) -> kind_of l

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
    | `Bop (_, _, ((`Mu _ | `Lam _) as t), _)
    | `Bop (_, _, _, ((`Mu _ | `Lam _) as t))
    | `For (_, _, t)
    | `App (_, t, _)
    | `Mu (_, t) ->
      fresh_from t
    | `Lam (_, i, _, t) -> (Var.freshen i, t)
    | t -> (Var.fresh (FomAST.Typ.at t), t)

  let rec fresh_from_n n t =
    if n <= 0 then []
    else fresh_from t |> fun (i, t) -> i :: fresh_from_n (n - 1) t
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

  let unfold at f mu xs =
    app_of_norm at f mu >>= fun f_mu -> apps_of_norm at f_mu xs

  let rec unfold_of_norm typ =
    match unapp typ with
    | (`Mu (at', f) as mu), xs -> unfold at' f mu xs >>= unfold_of_norm
    | _ -> return typ

  (* *)

  let rec ground t =
    t
    |> keep_phys_eq @@ function
       | `Lam (at', i, k, t) -> `Lam (at', i, Kind.ground k, ground t)
       | t -> map_eq ground t

  (* *)

  let rec resolve t =
    t
    |> keep_phys_eq_fr @@ function
       | `Lam (at', d, k, t) ->
         Kind.resolve k <*> resolve t >>- fun (k, t) -> `Lam (at', d, k, t)
       | t -> map_eq_fr resolve t
end

module GoalSet = Set.Make (struct
  type nonrec t = Var.t VarMap.t * Var.t VarMap.t * Core.t * Core.t

  let compare (l_env1, r_env1, l1, r1) (l_env2, r_env2, l2, r2) =
    Core.compare_in_env l_env1 l_env2 l1 l2 <>? fun () ->
    Core.compare_in_env r_env1 r_env2 r1 r2
end)

module Goals = struct
  type 'r m = (GoalSet.t MVar.t, 'r) Field.t
  type 'r f = < goals : 'r m >

  let empty () = MVar.create GoalSet.empty
  let field r = r#goals
  let resetting op = setting field (empty ()) op

  let adding g op =
    let* added =
      try_modify field @@ fun gs ->
      let gs' = GoalSet.add g gs in
      return (gs', gs != gs')
    in
    if added then op () else unit

  class con =
    object
      val goals = empty ()
      method goals : _ m = Field.make goals (fun v -> {<goals = v>})
    end
end

(* *)

module Solved = struct
  include Map.Make (FomAST.Typ)

  type 'r m = (FomAST.Typ.t t, 'r) Field.t
  type 'r f = < typ_solved : 'r m >

  let field r : _ m = r#typ_solved

  class con =
    object
      val typ_solved = empty

      method typ_solved : _ m =
        Field.make typ_solved (fun v -> {<typ_solved = v>})
    end
end

(* *)

let union op (ls, rs) =
  Row.union_fr (const return) (const return) (const op) ls rs

let intersection op =
  let rec loop os = function
    | ((ll, lt) :: lls as llls), ((rl, rt) :: rls as rlls) ->
      let c = Label.compare ll rl in
      if c < 0 then loop os (lls, rlls)
      else if 0 < c then loop os (llls, rls)
      else op lt rt >>= fun t -> loop ((ll, t) :: os) (lls, rls)
    | [], _ | _, [] -> return @@ List.rev os
  in
  loop []

let rop o m =
  match (o, m) with
  | `Join, `Product | `Meet, `Sum -> intersection
  | `Join, `Sum | `Meet, `Product -> union

let bop o = function
  | at1, `Bop (at2, o', a, b), c when o = o' ->
    `Bop (at1, o, a, `Bop (at2, o, b, c))
  | at', l, r -> `Bop (at', o, l, r)

let inv = function `Join -> `Meet | `Meet -> `Join

(* *)

let rec subset at' l_env r_env l r flip ls ms =
  match (ls, ms) with
  | [], _ -> unit
  | (ll, _) :: _, [] -> fail @@ `Error_label_missing (at', ll, l, r)
  | ((ll, lt) :: ls as lls), (ml, mt) :: ms ->
    let c = Label.compare ll ml in
    if c = 0 then
      flip (sub at' l_env r_env) mt lt >> subset at' l_env r_env l r flip ls ms
    else if 0 < c then subset at' l_env r_env l r flip lls ms
    else fail @@ `Error_label_missing (at', ll, l, r)

and eq at' l_env r_env l r = sub at' l_env r_env l r >> sub at' r_env l_env r l

and sub at' l_env r_env l r =
  let g = (l_env, r_env, l, r) in
  if Core.compare_in_env l_env r_env l r = 0 then unit
  else
    Goals.adding g @@ fun () ->
    match (unapp l, unapp r) with
    | (`Arrow (_, ld, lc), _), (`Arrow (_, rd, rc), _) ->
      sub at' r_env l_env rd ld >> sub at' l_env r_env lc rc
    | (`Row (_, m, lls), _), (`Row (_, m', rls), _) when m = m' ->
      let flip = match m with `Product -> id | `Sum -> Fun.flip in
      flip (flip (subset at' l_env r_env) r l flip) rls lls
    | (`Lam (_, li, lk, lt), _), (`Lam (_, ri, rk, rt), _) ->
      let v, l_env, r_env =
        if Var.equal li ri then
          (li, VarMap.remove li l_env, VarMap.remove ri r_env)
        else
          let v = Var.fresh Loc.dummy in
          (v, VarMap.add li v l_env, VarMap.add ri v r_env)
      in
      Kind.unify at' lk rk
      >> VarEnv.adding v (`Kind lk) (sub at' l_env r_env lt rt)
    | (`Var (_, l), (_ :: _ as lxs)), (`Var (_, r), (_ :: _ as rxs))
      when Var.equal l r && List.length lxs = List.length rxs ->
      List.iter2_fr (eq at' l_env r_env) lxs rxs
    | ((`Mu (la, lf) as lmu), lxs), _ ->
      Core.unfold la lf lmu lxs >>= fun l -> sub at' l_env r_env l r
    | _, ((`Mu (ra, rf) as rmu), rxs) ->
      Core.unfold ra rf rmu rxs >>= fun r -> sub at' l_env r_env l r
    | (`For (_, l_q, l), _), (`For (_, r_q, r), _) when l_q = r_q ->
      sub at' l_env r_env l r
    | _, (`For (_, `All, rf), _) ->
      let i, _ = Var.fresh_from (rf :> t) in
      let k = Kind.fresh at' in
      let* r = Core.app_of_norm at' rf @@ var i in
      kind_of rf
      >>= Kind.unify at' @@ `Arrow (at', k, `Star at')
      >> VarEnv.adding i (`Kind k) (sub at' l_env r_env l r)
    | _ -> fail @@ `Error_typ_mismatch (at', (r :> t), (l :> t))

let check_sub_of_norm at' l r =
  sub at' VarMap.empty VarMap.empty l r |> Goals.resetting

let check_equal_of_norm at' l r =
  eq at' VarMap.empty VarMap.empty l r |> Goals.resetting

let rec mu_of_norm at = function
  | `Lam (at', i, k, t) as f -> (
    is_free i t >>= function
    | false -> return t
    | true -> (
      match t with
      | `Mu (at1, `Lam (at2, j, k, t)) ->
        let+ t = subst_of_norm (VarMap.singleton i @@ var j) t in
        `Mu (at1, `Lam (at2, j, k, t))
      | _ ->
        let t', iks = unlam t in
        let* f' =
          match t' with
          | `Bop _ ->
            let mu' =
              List.fold_right (fun (i, _) f -> `App (at, f, var i)) iks (var i)
            in
            let+ t' = drop_legs mu' t' in
            let t' =
              List.fold_left (fun t (i, k) -> `Lam (at, i, k, t)) t' iks
            in
            `Lam (at', i, k, t')
          | _ -> unfold_at_jms i t t >>- fun t' -> `Lam (at', i, k, t')
        in
        if compare f f' = 0 then return @@ `Mu (at, f) else mu_of_norm at f'))
  | f -> return @@ `Mu (at, f)

and unfold_at_jms x f =
  keep_phys_eq_fr @@ function
  | #Core.f as t -> map_eq_fr (unfold_at_jms x f) t
  | `Bop (at', o, l, r) as t -> (
    let* l = unfold_at_jms x f l and* r = unfold_at_jms x f r in
    let op = bop_of_norm o at' in
    match (unapp l, unapp r) with
    | (`Var (_, lf), lxs), _ when Var.equal lf x ->
      apps_of_norm at' f lxs >>= fun l ->
      op l r >>= unfold_at_jms x f |> memoing t
    | _, (`Var (_, rf), rxs) when Var.equal rf x ->
      apps_of_norm at' f rxs >>= fun r ->
      op l r >>= unfold_at_jms x f |> memoing t
    | _ -> op l r)

and drop_legs x =
  keep_phys_eq_fr @@ function
  | `Bop (at', o, l, r) ->
    let* r = drop_legs x r in
    if compare x l = 0 then return r
    else if compare x r = 0 then return l
    else bop_of_norm o at' l r
  | t -> return t

and lam_of_norm at i k = function
  | `App (_, f, `Var (_, i')) as t' when Var.equal i i' -> (
    is_free i f >>- function false -> f | true -> `Lam (at, i, k, t'))
  | t' -> return @@ `Lam (at, i, k, t')

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
    unfold at' f (var mu') xs >>= classify |> VarEnv.adding mu' @@ `Kind k
  | `Var _, _ -> return None
  | `Const (_, c), _ -> return @@ Some (`Const c)
  | `Lam _, _ -> return @@ Some `Lam
  | `Arrow _, _ -> return @@ Some `Arrow
  | `For (_, q, _), _ -> return @@ Some (q : [`All | `Unk] :> [> `All | `Unk])
  | `Row (_, m, _), _ ->
    return @@ Some (m : [`Product | `Sum] :> [> `Product | `Sum])
  | `Bop (_, _, l, r), _ -> (
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
    kind_of t >>= fun k -> lam_of_norm at' i k t' >>= mu_of_norm at'

and bop_of_norm o at' l r =
  if compare l r = 0 then return l
  else
    match (l, r) with
    | `Arrow (_, ld, lc), `Arrow (_, rd, rc) ->
      bop_of_norm (inv o) at' ld rd <*> bop_of_norm o at' lc rc
      >>- fun (d, c) -> `Arrow (at', d, c)
    | `Row (_, m, lls), `Row (_, m', rls) when m = m' ->
      rop o m (bop_of_norm o at') (lls, rls) >>- fun ls -> `Row (at', m, ls)
    | `Lam (_, li, lk, lt), `Lam (_, ri, rk, rt) ->
      let* i, lt, rt =
        if Var.equal li ri then return (li, lt, rt)
        else
          is_free li rt >>= function
          | false ->
            subst_of_norm (VarMap.singleton ri (var li)) rt >>- fun rt ->
            (li, lt, rt)
          | true -> (
            is_free ri lt >>= function
            | false ->
              subst_of_norm (VarMap.singleton li (var ri)) lt >>- fun lt ->
              (ri, lt, rt)
            | true ->
              let i = Var.freshen li in
              let v = var i in
              let+ lt = subst_of_norm (VarMap.singleton li v) lt
              and+ rt = subst_of_norm (VarMap.singleton ri v) rt in
              (i, lt, rt))
      in
      Kind.unify at' lk rk >> bop_of_norm o at' lt rt
      |> VarEnv.adding i @@ `Kind lk
      >>- fun t -> `Lam (at', i, lk, t)
    | `For (_, q, lt), `For (_, q', rt) when q = q' ->
      bop_of_norm o at' lt rt >>- fun t -> `For (at', q, t)
    | _ -> (
      let problem = bop o (at', l, r) in
      match (unapp l, unapp r) with
      | ((`Mu (la, lf) as lmu), lxs), _ ->
        unfold la lf lmu lxs >>= fun l ->
        bop_of_norm o at' l r |> memoing problem
      | _, ((`Mu (ra, rf) as rmu), rxs) ->
        unfold ra rf rmu rxs >>= fun r ->
        bop_of_norm o at' l r |> memoing problem
      | _ -> (
        classify l <*> classify r >>= fun (l', r') ->
        match Option.both ( = ) l' r' with
        | Some false -> fail @@ `Error_typ_unrelated (at', l, r)
        | _ -> return problem))

and subst_of_norm env =
  keep_phys_eq_fr @@ function
  | `Var (_, i) as inn ->
    VarMap.find_opt i env |> Option.value ~default:inn |> return
  | `Mu (at, t) -> subst_of_norm env t >>= mu_of_norm at
  | `Lam (at, i, k, t) as inn -> (
    let env = VarMap.remove i env in
    if VarMap.is_empty env then return inn
    else
      VarMap.exists_fr (fun i' t' -> is_free i t' &&& is_free i' t) env
      >>= function
      | true ->
        let i' = Var.freshen i in
        let v' = `Var (at, i') in
        subst_of_norm (VarMap.add i v' env) t
        |> VarEnv.adding i' @@ `Kind k
        >>= lam_of_norm at i' k
      | false ->
        subst_of_norm env t |> VarEnv.adding i @@ `Kind k >>= lam_of_norm at i k
    )
  | `App (at, f, x) ->
    subst_of_norm env f <*> subst_of_norm env x >>= fun (f, x) ->
    app_of_norm at f x
  | `Bop (at', o, l, r) ->
    subst_of_norm env l <*> subst_of_norm env r >>= fun (l, r) ->
    bop_of_norm o at' l r
  | t -> map_eq_fr (subst_of_norm env) t

(* *)

let rec find_map_from_all_apps_of i' p = function
  | `Lam (_, i, _, t) ->
    if Var.equal i i' then return None else find_map_from_all_apps_of i' p t
  | (`App _ | `Var _) as t -> (
    match unapp t with
    | (`Var (_, i) as f), xs ->
      if Var.equal i i' then
        p t f xs >>= function
        | None -> xs |> List.find_map_fr (find_map_from_all_apps_of i' p)
        | some -> return some
      else xs |> List.find_map_fr (find_map_from_all_apps_of i' p)
    | f, xs -> (
      find_map_from_all_apps_of i' p f >>= function
      | None -> xs |> List.find_map_fr (find_map_from_all_apps_of i' p)
      | some -> return some))
  | t -> find_map_fr (find_map_from_all_apps_of i' p) t

let find_opt_nested_arg_mu at f arity =
  if arity <= 0 then return None
  else
    let i = Var.fresh at in
    let v = var i in
    let is = List.init arity @@ fun _ -> Var.fresh at in
    let vs = is |> List.map var in
    app_of_norm at f v >>= fun fv ->
    apps_of_norm at fv vs
    >>= find_map_from_all_apps_of i @@ fun _ _ xs ->
        xs
        |> List.find_map_fr @@ function
           | `Var _ -> return None
           | t -> (
             is
             |> List.find_map_fr @@ fun i ->
                is_free i t >>- function true -> Some t | false -> None)

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
  t
  |> keep_phys_eq_fr @@ function
     | `Lam (at', d, k, t) ->
       Kind.resolve k <*> resolve t >>- fun (k, t) -> `Lam (at', d, k, t)
     | t -> map_eq_fr resolve t

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
  | `Var (_, i) as t -> return t <*> VarEnv.kind_of i
  | `Lam (at', d, d_kind, r) ->
    let* r, r_kind = infer r |> VarEnv.adding d @@ `Kind d_kind
    and* d_kind = Kind.resolve d_kind in
    lam_of_norm at' d d_kind r <*> return @@ `Arrow (at', d_kind, r_kind)
  | `App (at', f, x) ->
    let* f, f_kind = infer f and* x, d_kind = infer x in
    let c_kind = Kind.fresh at' in
    Kind.unify at' (`Arrow (at', d_kind, c_kind)) f_kind
    >> (app_of_norm at' f x <*> return c_kind)
  | `For (at', q, f) ->
    let* f, f_kind = infer f in
    let d_kind = Kind.fresh at' and c_kind = `Star at' in
    Kind.unify at' (`Arrow (at', d_kind, c_kind)) f_kind
    >> return (`For (at', q, f), c_kind)
  | `Arrow (at', d, c) ->
    let star = `Star at' in
    check star d <*> check star c >>- fun (d, c) -> (`Arrow (at', d, c), star)
  | `Row (at', m, ls) ->
    let star = `Star at' in
    let+ ls = Row.check ls >> Row.map_fr (check star) ls in
    (`Row (at', m, ls), star)
  | `Bop (at', o, l, r) ->
    infer l <*> infer r >>= fun ((l, lk), (r, rk)) ->
    Kind.unify at' lk rk >> (bop_of_norm o at' l r <*> return lk)

and check expected t =
  let* t, actual = infer t in
  Kind.unify (at t) expected actual >> return t

(* *)

let rec solve_of_norm = function
  | #Core.f as t -> Core.map_fr solve_of_norm t
  | `Bop (at', _, l, r) -> fail @@ `Error_typ_unrelated (at', l, r)

let join_of_norm at' (l : Core.t) (r : Core.t) =
  bop_of_norm `Join at' (l :> t) (r :> t) >>= solve_of_norm

(* *)

let infer t = infer t >>= Pair.map_fr solve_of_norm return
let check k = check k >=> solve_of_norm

(* *)

let infer_and_resolve t = infer t >>= (fst >>> Core.resolve)
let check_and_resolve k = check k >=> Core.resolve

(* *)

let as_predicate check l r =
  check Loc.dummy l r
  |> try_in (const @@ return true) (const @@ return false)
  |> Kind.UnkEnv.cloning

let is_sub_of_norm l r = as_predicate check_sub_of_norm l r
let is_equal_of_norm l r = as_predicate check_equal_of_norm l r
