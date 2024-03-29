open Rea
open StdlibPlus
open FomSource

(* *)

include FomPP.Typ
include FomAST.Typ

(* *)

module VarEnv = struct
  type 't m = 't VarMap.t Prop.t

  let field r = r#typ_env

  let find i =
    get_as field @@ VarMap.find_opt i >>= function
    | Some t -> pure t
    | None -> fail @@ `Error_typ_var_unbound (Var.at i, i)

  let kind_of i =
    get_as field @@ VarMap.find_opt i >>= function
    | Some (`Kind k) -> pure k
    | _ -> fail @@ `Error_typ_var_unbound (Var.at i, i)

  let existing pr = get_as field @@ VarMap.exists pr
  let existing_er pr = get field >>= VarMap.exists_er pr
  let resetting_to initial op = setting field initial op
  let adding i v = mapping field @@ VarMap.add i v
  let merging env = mapping field (VarMap.merge Map.prefer_lhs env)

  class ['t] con =
    object
      val mutable typ_env = VarMap.empty
      method typ_env : 't m = prop (fun () -> typ_env) (fun x -> typ_env <- x)
    end
end

(* *)

let to_apps = function
  | (`Arrow _ | `Const _ | `For _ | `Lam _ | `Row _) as t -> t
  | t -> `Apps (unapp t)

(* *)

let rec kind_of = function
  | `Mu (_, f) | `App (_, f, _) -> (
    kind_of f >>= Kind.resolve >>- function
    | `Star _ | `Unk _ -> failwith "kind_of cod"
    | `Arrow (_, _, c) -> c)
  | `Const (at', c) -> pure'2 Const.kind_of at' c
  | `Var (_, i) -> VarEnv.kind_of i
  | `Lam (at', i, d, t) ->
    kind_of t |> VarEnv.adding i @@ `Kind d >>- fun c -> `Arrow (at', d, c)
  | `Arrow (at', _, _) | `For (at', _, _) | `Row (at', _, _) ->
    pure @@ `Star at'
  | `Bop (_, _, l, _) -> kind_of l

let kind_of t = kind_of t >>= Kind.resolve

(* *)

module Var = struct
  include Var

  let rec from = function
    | `Bop (_, _, ((`Mu _ | `Lam _) as t), _)
    | `Bop (_, _, _, ((`Mu _ | `Lam _) as t))
    | `For (_, _, t)
    | `App (_, t, _)
    | `Mu (_, t) ->
      from t
    | `Lam (_, i, _, _) -> i
    | t -> of_string (FomAST.Typ.at t) "v"
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

  let rec unfold_of_norm t =
    eta'0 @@ fun () ->
    match to_apps t with
    | `Apps ((`Mu (at', f) as mu), xs) -> unfold at' f mu xs >>= unfold_of_norm
    | _ -> pure t

  (* *)

  let rec ground t =
    t
    |> keep_eq @@ function
       | `Lam (at', i, k, t) -> `Lam (at', i, Kind.ground k, ground t)
       | t -> map_eq ground t

  (* *)

  let rec resolve t =
    t
    |> keep_eq_er @@ function
       | `Lam (at', d, k, t) ->
         Kind.resolve k <*> resolve t >>- fun (k, t) -> `Lam (at', d, k, t)
       | t -> map_eq_er resolve t
end

module GoalSet = Set.Make (Compare.Tuple'2 (Core) (Core))

module Goals = struct
  type m = GoalSet.t Mut.t Prop.t

  let empty () = Mut.create GoalSet.empty
  let field r : m = r#goals
  let resetting op = setting field (empty ()) op

  let adding g op =
    let* added =
      try_modify field @@ fun gs ->
      let gs' = GoalSet.add g gs in
      pure (gs', gs != gs')
    in
    if added then op () else unit

  class con =
    object
      val mutable goals = empty ()
      method goals : m = prop (fun () -> goals) (fun x -> goals <- x)
    end
end

(* *)

module Solved = struct
  include Map.Make (FomAST.Typ)

  type m = FomAST.Typ.t t Prop.t

  let field r : m = r#solved

  class con =
    object
      val mutable solved = empty
      method solved : m = prop (fun () -> solved) (fun x -> solved <- x)
    end
end

(* *)

let rec solve_of_norm = function
  | #Core.f as t -> Core.map_er solve_of_norm t
  | `Bop (at', _, l, r) -> fail @@ `Error_typ_unrelated (at', l, r)

(* *)

let union _ _ _ op (ls, rs) =
  Row.union_er (const pure) (const pure) (const op) ls rs

let intersection _ _ _ op =
  let rec loop os = function
    | ((ll, lt) :: lls as llls), ((rl, rt) :: rls as rlls) ->
      let c = Label.compare ll rl in
      if c < 0 then loop os (lls, rlls)
      else if 0 < c then loop os (llls, rls)
      else op lt rt >>= fun t -> loop ((ll, t) :: os) (lls, rls)
    | [], _ | _, [] -> pure'1 List.rev os
  in
  loop []

let matching at' l r op =
  let rec loop os = function
    | (ll, lt) :: lls, (rl, rt) :: rls ->
      let c = Label.compare ll rl in
      if c < 0 then fail @@ `Error_label_missing (at', ll, l, r)
      else if 0 < c then fail @@ `Error_label_missing (at', rl, r, l)
      else op lt rt >>= fun t -> loop ((ll, t) :: os) (lls, rls)
    | (ll, _) :: _, [] -> fail @@ `Error_label_missing (at', ll, l, r)
    | [], (rl, _) :: _ -> fail @@ `Error_label_missing (at', rl, r, l)
    | [], [] -> pure'1 List.rev os
  in
  loop []

let rop o m =
  match (o, m) with
  | `Join, `Product | `Meet, `Sum -> intersection
  | `Join, `Sum | `Meet, `Product -> union
  | `Eq, (`Product | `Sum) -> matching

let bop o = function
  | at1, `Bop (at2, o', a, b), c when o = o' ->
    `Bop (at1, o, a, `Bop (at2, o, b, c))
  | at', l, r -> `Bop (at', o, l, r)

let inv = function `Join -> `Meet | `Meet -> `Join | `Eq -> `Eq

(* *)

let bop_counter = Profiling.Counter.register "bop"

let rec mu_of_norm at = function
  | `Lam (at', i, k, t) as f -> (
    is_free i t >>= function
    | false -> pure t
    | true -> (
      match t with
      | `Mu (at1, `Lam (at2, j, k, t)) ->
        let+ t = subst_of_norm i (var j) t in
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
        if compare f f' = 0 then pure @@ `Mu (at, f) else mu_of_norm at f'))
  | f -> pure @@ `Mu (at, f)

and unfold_at_jms x f =
  keep_eq_er @@ function
  | #Core.f as t -> map_eq_er (unfold_at_jms x f) t
  | `Bop (at', o, l, r) as t -> (
    let* l = unfold_at_jms x f l and* r = unfold_at_jms x f r in
    let op = bop_of_norm o at' in
    match (unapp l, unapp r) with
    | (`Var (_, lf), lxs), _ when Var.equal lf x ->
      memoing t @@ fun () ->
      apps_of_norm at' f lxs >>= fun l -> op l r >>= unfold_at_jms x f
    | _, (`Var (_, rf), rxs) when Var.equal rf x ->
      memoing t @@ fun () ->
      apps_of_norm at' f rxs >>= fun r -> op l r >>= unfold_at_jms x f
    | _ -> op l r)

and drop_legs x =
  keep_eq_er @@ function
  | `Bop (at', o, l, r) ->
    let* r = drop_legs x r in
    if compare x l = 0 then pure r
    else if compare x r = 0 then pure l
    else bop_of_norm o at' l r
  | t -> pure t

and lam_of_norm at i k = function
  | `App (_, f, `Var (_, i')) as t' when Var.equal i i' -> (
    is_free i f >>- function false -> f | true -> `Lam (at, i, k, t'))
  | t' -> pure @@ `Lam (at, i, k, t')

and app_of_norm at f' x' =
  match f' with
  | `Lam (_, i, _, t) -> subst_of_norm i x' t
  | f' -> pure @@ `App (at, f', x')

and apps_of_norm at' = List.fold_left_er (app_of_norm at')

and unfold at f mu xs =
  app_of_norm at f mu >>= fun f_mu -> apps_of_norm at f_mu xs

and classify t =
  eta'0 @@ fun () ->
  match to_apps t with
  | `Apps ((`Mu (at', f) as mu), xs) ->
    let mu' = Var.from f |> Var.freshen in
    let* k = kind_of mu in
    unfold at' f (var mu') xs >>= classify |> VarEnv.adding mu' @@ `Kind k
  | `Apps (`Var _, _) -> pure None
  | `Apps (`Bop (_, _, l, r), _) -> (
    classify l >>= function None -> classify r | some -> pure some)
  | `Arrow _ -> pure @@ Some `Arrow
  | `Const (_, c) -> pure @@ Some (`Const c)
  | `For (_, q, _) -> pure @@ Some (q : [`All | `Unk] :> [> `All | `Unk])
  | `Lam _ -> pure @@ Some `Lam
  | `Row (_, m, _) ->
    pure @@ Some (m : [`Product | `Sum] :> [> `Product | `Sum])
  | _ -> failwith "classify"

and memoing t op =
  get Solved.field >>- Solved.find_opt t >>= function
  | Some t -> pure t
  | None ->
    let at' = at t in
    let i = Var.from t |> Var.freshen in
    let* k = kind_of t in
    op ()
    |> VarEnv.adding i @@ `Kind k
    |> mapping Solved.field @@ Solved.add t (var i)
    >>= lam_of_norm at' i k >>= mu_of_norm at'

and bop_of_norm o at' l r =
  memoing (bop o (at', l, r)) @@ fun () ->
  Profiling.Counter.inc bop_counter;
  let head l r =
    match (l, r) with
    | `Var (_, l'), `Var (_, r') when Var.equal l' r' -> pure l
    | _ -> fail @@ `Error_typ_unrelated (at', (l :> t), (r :> t))
  in
  match (to_apps l, to_apps r) with
  | `Apps ((`Mu (la, lf) as lmu), lxs), _ ->
    unfold la lf lmu lxs >>= fun l -> bop_of_norm o at' l r
  | _, `Apps ((`Mu (ra, rf) as rmu), rxs) ->
    unfold ra rf rmu rxs >>= fun r -> bop_of_norm o at' l r
  | `Apps (lf, lxs), `Apps (rf, rxs) when List.length lxs = List.length rxs ->
    head lf rf <*> List.map_er'2 (bop_of_norm `Eq at') lxs rxs
    >>= fun (f, xs) -> apps_of_norm at' f xs
  | `Arrow (_, ld, lc), `Arrow (_, rd, rc) ->
    bop_of_norm (inv o) at' ld rd <*> bop_of_norm o at' lc rc >>- fun (d, c) ->
    `Arrow (at', d, c)
  | `Const (_, lc), `Const (_, rc) when Const.equal lc rc -> pure l
  | `For (_, q, lt), `For (_, q', rt) when q = q' ->
    bop_of_norm o at' lt rt >>- fun t -> `For (at', q, t)
  | `Lam (_, li, lk, lt), `Lam (_, ri, rk, rt) ->
    let* i =
      li |> Var.Unsafe.smallest @@ fun i -> is_free i l ||| is_free i r
    in
    let v = var i in
    let* lt = subst_of_norm li v lt and* rt = subst_of_norm ri v rt in
    Kind.unify at' lk rk >> bop_of_norm o at' lt rt
    |> VarEnv.adding i @@ `Kind lk
    >>- fun t -> `Lam (at', i, lk, t)
  | `Row (_, m, lls), `Row (_, m', rls) when m = m' ->
    rop o m at' l r (bop_of_norm o at') (lls, rls) >>- fun ls ->
    `Row (at', m, ls)
  | _ -> (
    classify l <*> classify r >>- uncurry (Option.both ( = )) >>= function
    | Some false -> fail @@ `Error_typ_unrelated (at', l, r)
    | _ -> pure'2 bop o (at', l, r))

and subst_of_norm i v t =
  let rec subst_of_norm env =
    keep_eq_er @@ function
    | `Var (_, i) as inn ->
      VarMap.find_opt i env |> Option.value ~default:inn |> pure
    | `Mu (at, t) -> subst_of_norm env t >>= mu_of_norm at
    | `Lam (at, i, k, t) as inn ->
      let env = VarMap.remove i env in
      if VarMap.is_empty env then pure inn
      else
        let* i' =
          i
          |> Var.Unsafe.smallest @@ fun i ->
             is_free i inn
             ||| VarMap.exists_er
                   (fun i' t' -> is_free i t' &&& is_free i' inn)
                   env
        in
        if Var.equal i i' then subst_of_norm env t >>= lam_of_norm at i k
        else subst_of_norm (VarMap.add i (var i') env) t >>= lam_of_norm at i' k
    | `App (at, f, x) ->
      subst_of_norm env f <*> subst_of_norm env x >>= uncurry @@ app_of_norm at
    | `Bop (at', o, l, r) ->
      subst_of_norm env l <*> subst_of_norm env r
      >>= uncurry @@ bop_of_norm o at'
    | t -> map_eq_er (subst_of_norm env) t
  in
  match v with
  | `Var (_, i') when Var.equal i i' -> pure t
  | v -> subst_of_norm (VarMap.singleton i v) t

(* *)

let sub_counter = Profiling.Counter.register "sub"

let rec subset at' (l : t) (r : t) flip ls ms =
  match (ls, ms) with
  | [], _ -> unit
  | (ll, _) :: _, [] -> fail @@ `Error_label_missing (at', ll, l, r)
  | ((ll, lt) :: ls as lls), (ml, mt) :: ms ->
    let c = Label.compare ll ml in
    if c = 0 then flip (sub at') mt lt >> subset at' l r flip ls ms
    else if 0 < c then subset at' l r flip lls ms
    else fail @@ `Error_label_missing (at', ll, l, r)

and eq at' l r = sub at' l r >> sub at' r l

and sub at' l r =
  Goals.adding (l, r) @@ fun () ->
  Profiling.Counter.inc sub_counter;
  match (to_apps l, to_apps r) with
  | `Apps ((`Mu (la, lf) as lmu), lxs), _ ->
    Core.unfold la lf lmu lxs >>= fun l -> sub at' l r
  | _, `Apps ((`Mu (ra, rf) as rmu), rxs) ->
    Core.unfold ra rf rmu rxs >>= fun r -> sub at' l r
  | `Apps (`Var (_, li), lxs), `Apps (`Var (_, ri), rxs)
    when Var.equal li ri && List.length lxs = List.length rxs ->
    List.iter_er'2 (eq at') lxs rxs
  | `Arrow (_, ld, lc), `Arrow (_, rd, rc) -> sub at' rd ld >> sub at' lc rc
  | `Const (_, lc), `Const (_, rc) when Const.equal lc rc -> unit
  | `For (_, lq, l), `For (_, rq, r) when lq = rq -> sub at' l r
  | _, `For (_, `All, rf) ->
    let i = Var.from (rf :> t) |> Var.freshen in
    let k = Kind.fresh at' in
    let* r = Core.app_of_norm at' rf @@ var i in
    kind_of rf
    >>= Kind.unify at' @@ `Arrow (at', k, `Star at')
    >> VarEnv.adding i (`Kind k) (sub at' l r)
  | `Lam (_, li, lk, lt), `Lam (_, ri, rk, rt) ->
    let* i =
      li
      |> Var.Unsafe.smallest @@ fun i -> Core.is_free i l ||| Core.is_free i r
    in
    let v = var i in
    let* lt = Core.subst_of_norm li v lt and* rt = Core.subst_of_norm ri v rt in
    Kind.unify at' lk rk >> sub at' lt rt |> VarEnv.adding i @@ `Kind lk
  | `Row (_, m, lls), `Row (_, m', rls) when m = m' ->
    let flip = match m with `Product -> id | `Sum -> Fun.flip in
    flip (flip (subset at') (r :> t) (l :> t) flip) rls lls
  | _ -> fail @@ `Error_typ_mismatch (at', (r :> t), (l :> t))

let check_sub_of_norm at' l r = sub at' l r |> Goals.resetting
let check_equal_of_norm at' l r = eq at' l r |> Goals.resetting

(* *)

let find_map_from_all_apps_of i' p =
  let rec find_map_from_all_apps_of = function
    | `Lam (_, i, _, t) ->
      if Var.equal i i' then pure None else find_map_from_all_apps_of t
    | (`App _ | `Var _) as t -> (
      match unapp t with
      | (`Var (_, i) as f), xs ->
        if Var.equal i i' then
          p t f xs >>= function
          | None -> xs |> List.find_map_er find_map_from_all_apps_of
          | some -> pure some
        else xs |> List.find_map_er find_map_from_all_apps_of
      | f, xs -> (
        find_map_from_all_apps_of f >>= function
        | None -> xs |> List.find_map_er find_map_from_all_apps_of
        | some -> pure some))
    | t -> find_map_er find_map_from_all_apps_of t
  in
  find_map_from_all_apps_of

let find_opt_nested_arg_mu at' f arity =
  if arity <= 0 then pure None
  else
    let i = Var.fresh at' in
    let is = List.init arity @@ fun _ -> Var.fresh at' in
    app_of_norm at' f (var i) >>= fun fv ->
    apps_of_norm at' fv (List.map var is)
    >>= find_map_from_all_apps_of i @@ fun _ _ xs ->
        xs
        |> List.find_map_er @@ function
           | `Var _ -> pure None
           | t -> (
             is
             |> List.find_map_er @@ fun i ->
                is_free i t >>- function true -> Some t | false -> None)

(* *)

let rec find_opt_non_contractive is t =
  match unapp t with
  | `Mu (_, `Lam (_, i, _, f)), xs -> (
    apps_of_norm Loc.dummy f xs >>- unapp >>= function
    | (`Var (_, i') as mu), _ when Var.equal i' i || VarSet.mem i' is ->
      pure @@ Some mu
    | t, _ -> find_opt_non_contractive (VarSet.add i is) t)
  | _ -> pure None

let find_opt_non_contractive_mu at' f arity =
  match f with
  | `Lam (_, i, _, f) -> (
    let xs = List.init arity @@ fun _ -> var @@ Var.fresh at' in
    apps_of_norm Loc.dummy f xs >>- unapp >>= function
    | (`Var (_, i') as mu), _ when Var.equal i' i -> pure @@ Some mu
    | t, _ -> find_opt_non_contractive (VarSet.singleton i) t)
  | _ -> pure None

(* *)

let rec resolve t =
  t
  |> keep_eq_er @@ function
     | `Lam (at', d, k, t) ->
       Kind.resolve k <*> resolve t >>- fun (k, t) -> `Lam (at', d, k, t)
     | t -> map_eq_er resolve t

(* *)

let rec infer = function
  | `Mu (at', f) as typ ->
    let* f, f_kind = infer f in
    let kind = Kind.fresh at' in
    Kind.unify at' (`Arrow (at', kind, kind)) f_kind
    >> let* arity = Kind.resolve kind >>- Kind.min_arity in
       find_opt_nested_arg_mu at' f arity
       >>= Option.iter_er (fun typ' ->
               let* typ = resolve typ and* typ' = resolve typ' in
               fail @@ `Error_mu_nested (at', typ, typ'))
       >> (find_opt_non_contractive_mu at' f arity
          >>= Option.iter_er (fun typ' ->
                  let* typ = resolve typ and* typ' = resolve typ' in
                  fail @@ `Error_mu_non_contractive (at', typ, typ')))
       >> (mu_of_norm at' f <*> pure kind)
  | `Const (at', c) as t -> pure @@ (t, Const.kind_of at' c)
  | `Var (_, i) as t -> pure t <*> VarEnv.kind_of i
  | `Lam (at', d, d_kind, r) ->
    let* r, r_kind = infer r |> VarEnv.adding d @@ `Kind d_kind
    and* d_kind = Kind.resolve d_kind in
    lam_of_norm at' d d_kind r <*> pure @@ `Arrow (at', d_kind, r_kind)
  | `App (at', f, x) ->
    let* f, f_kind = infer f and* x, d_kind = infer x in
    let c_kind = Kind.fresh at' in
    Kind.unify at' (`Arrow (at', d_kind, c_kind)) f_kind
    >> (app_of_norm at' f x <*> pure c_kind)
  | `For (at', q, f) ->
    let* f, f_kind = infer f in
    let d_kind = Kind.fresh at' and c_kind = `Star at' in
    Kind.unify at' (`Arrow (at', d_kind, c_kind)) f_kind
    >> pure (`For (at', q, f), c_kind)
  | `Arrow (at', d, c) ->
    let star = `Star at' in
    check star d <*> check star c >>- fun (d, c) -> (`Arrow (at', d, c), star)
  | `Row (at', m, ls) ->
    let star = `Star at' in
    let+ ls = Row.check ls >> Row.map_er (check star) ls in
    (`Row (at', m, ls), star)
  | `Bop (at', o, l, r) ->
    infer l <*> infer r >>= fun ((l, lk), (r, rk)) ->
    Kind.unify at' lk rk >> (bop_of_norm o at' l r <*> pure lk)

and check expected t =
  let* t, actual = infer t in
  Kind.unify (at t) expected actual >> pure t

(* *)

let join_of_norm at' (l : Core.t) (r : Core.t) =
  bop_of_norm `Join at' (l :> t) (r :> t) >>= solve_of_norm

(* *)

let infer t = infer t >>= Pair.map_er solve_of_norm pure
let check k = check k >=> solve_of_norm

(* *)

let infer_and_resolve t = infer t >>= (fst >>> Core.resolve)
let check_and_resolve k = check k >=> Core.resolve

(* *)

let as_predicate check l r =
  check Loc.dummy l r
  |> tryin (const @@ pure false) (const @@ pure true)
  |> Kind.UnkEnv.cloning

let is_sub_of_norm l r = as_predicate check_sub_of_norm l r
let is_equal_of_norm l r = as_predicate check_equal_of_norm l r
