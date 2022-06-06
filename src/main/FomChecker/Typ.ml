open FomSource
open FomBasis

(* *)

include FomPP.Typ
include FomAST.Typ

(* *)

module Unk = struct
  include Unk

  let from_var v = of_name (Var.at v) (Var.name v)

  let rec from = function
    | `Lam (_, i, _, _) -> from_var i
    | `Mu (_, t) -> from t
    | t -> of_string (FomAST.Typ.at t) "u"
end

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

let to_apps = function
  | (`Arrow _ | `Const _ | `For _ | `Lam _ | `Row _) as t -> t
  | t -> `Apps (unapp t)

(* *)

let rec kind_of = function
  | `Mu (_, f) | `App (_, f, _) -> (
    kind_of f >>= Kind.resolve >>- function
    | `Star _ | `Unk _ -> failwith "kind_of cod"
    | `Arrow (_, _, c) -> c)
  | `Const (at', c) -> return @@ Const.kind_of at' c
  | `Var (_, i) -> VarEnv.kind_of i
  | `Lam (at', i, d, t) ->
    kind_of t |> VarEnv.adding i @@ `Kind d >>- fun c -> `Arrow (at', d, c)
  | `Unk (_, x) -> UnkEnv.find x >>- fun (_, (k, _), _) -> k
  | `Arrow (at', _, _) | `For (at', _, _) | `Row (at', _, _) ->
    return @@ `Star at'
  | `Bop (_, _, l, _) -> kind_of l

let kind_of t = kind_of t >>= Kind.resolve

let kind_of_dom f =
  kind_of f >>- function
  | `Star _ -> failwith "kind_of_dom *"
  | `Unk _ -> failwith "kind_of_dom ?"
  | `Arrow (_, d, _) -> d

(* *)

module Var = struct
  include Var

  let from_unk u = Var.of_name (Unk.at u) (Unk.name u)

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
    match to_apps t with
    | `Apps ((`Mu (at', f) as mu), xs) -> unfold at' f mu xs >>= unfold_of_norm
    | _ -> return t

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

  (* *)

  let rec variance_of x' = function
    | `Unk (_, x) when Unk.equal x x' -> `Pos
    | `Arrow (_, d, c) ->
      Variance.join (Variance.neg (variance_of x' d)) (variance_of x' c)
    | t -> map_reduce Variance.join `None (variance_of x') t

  (* *)

  let rec replace_of_norm x' t' t =
    t
    |> keep_phys_eq_fr @@ function
       | `Unk (_, x) as inn -> (
         if Unk.equal x x' then return t'
         else
           UnkEnv.find x >>= function
           | Some t, _, _ ->
             let* t = UnkEnv.set x None >> replace_of_norm x' t' t in
             UnkEnv.set x (Some t) >> return inn
           | _ -> return inn)
       | t -> map_eq_fr (replace_of_norm x' t') t

  let rec update_unks_of_norm x' t' = function
    | `Unk (_, x) -> (
      if Unk.equal x x' then unit
      else
        UnkEnv.find x >>= function
        | Some t, _, _ ->
          let* t = UnkEnv.set x None >> replace_of_norm x' t' t in
          UnkEnv.set x (Some t)
        | _ -> unit)
    | t -> iter_fr (update_unks_of_norm x' t') t
end

module GoalSet = Set.Make (Compare.Tuple'2 (Core) (Core))

module Goals = struct
  type 'r m = (GoalSet.t MVar.t, 'r) Field.t
  type 'r f = < goals : 'r m >

  let empty () = MVar.create GoalSet.empty
  let field r = r#goals
  let resetting op = setting field (empty ()) op
  let cloning op = read field >>= fun v -> setting field (MVar.create v) op

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

let rec variance_of x' = function
  | `Unk (_, x) when Unk.equal x x' -> `Pos
  | `Arrow (_, d, c) ->
    Variance.join (Variance.neg (variance_of x' d)) (variance_of x' c)
  | t -> map_reduce Variance.join `None (variance_of x') t

let rec replace_of_norm x' (t' : Core.t) t =
  t
  |> keep_phys_eq_fr @@ function
     | `Unk (_, x) as inn -> (
       if Unk.equal x x' then return (t' :> t)
       else
         UnkEnv.find x >>= function
         | Some t, _, _ ->
           let* t = UnkEnv.set x None >> Core.replace_of_norm x' t' t in
           UnkEnv.set x (Some t) >> return inn
         | _ -> return inn)
     | t -> map_eq_fr (replace_of_norm x' t') t

let rec solve_of_norm = function
  | `Unk (at', x) -> (
    UnkEnv.deref x >>= function
    | x, (Some t, (k, _), _) ->
      let i = Var.from_unk x |> Var.freshen in
      let v = var i in
      UnkEnv.set x (Some v)
      >> solve_of_norm (t :> t)
      |> VarEnv.adding i @@ `Kind k
      >>= Core.lam_of_norm at' i k >>= Core.mu_of_norm at'
      >>= fun t -> UnkEnv.set x (Some t) >> return t
    | _, (None, _, _) -> failwithf "solve_of_norm %s" (Unk.to_string x))
  | #Core.f as t -> Core.map_fr solve_of_norm t
  | `Bop (at', _, l, r) -> fail @@ `Error_typ_unrelated (at', l, r)

let rec presolve_of_norm = function
  | `Unk (_, x) as t -> (
    UnkEnv.deref x >>- function
    | _, (Some ((`Var _ | `Const _) as t), _, _) -> t
    | x', _ -> if Unk.equal x x' then t else unk x')
  | #Core.f as t -> Core.map_fr presolve_of_norm t
  | `Bop (at', _, l, r) -> fail @@ `Error_typ_unrelated (at', l, r)

(* *)

let union _ _ _ op (ls, rs) =
  Row.union_fr (const return) (const return) (const op) ls rs

let intersection _ _ _ op =
  let rec loop os = function
    | ((ll, lt) :: lls as llls), ((rl, rt) :: rls as rlls) ->
      let c = Label.compare ll rl in
      if c < 0 then loop os (lls, rlls)
      else if 0 < c then loop os (llls, rls)
      else op lt rt >>= fun t -> loop ((ll, t) :: os) (lls, rls)
    | [], _ | _, [] -> return @@ List.rev os
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
    | [], [] -> return @@ List.rev os
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
let dual = function `Unk -> `All | `All -> `Unk
let bop_of = function `All -> `Join | `Unk -> `Meet

(* *)

let bop_counter = Profiling.Counter.register "bop"

let rec mu_of_norm at = function
  | `Lam (at', i, k, t) as f -> (
    is_free i t >>= function
    | false -> return t
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
      memoing t @@ fun () ->
      apps_of_norm at' f lxs >>= fun l -> op l r >>= unfold_at_jms x f
    | _, (`Var (_, rf), rxs) when Var.equal rf x ->
      memoing t @@ fun () ->
      apps_of_norm at' f rxs >>= fun r -> op l r >>= unfold_at_jms x f
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
  | `Lam (_, i, _, t) -> subst_of_norm i x' t
  | f' -> return @@ `App (at, f', x')

and apps_of_norm at' = List.fold_left_fr (app_of_norm at')

and unfold at f mu xs =
  app_of_norm at f mu >>= fun f_mu -> apps_of_norm at f_mu xs

and classify t =
  match to_apps t with
  | `Apps ((`Mu (at', f) as mu), xs) ->
    let mu' = Var.from f |> Var.freshen in
    let* k = kind_of mu in
    unfold at' f (var mu') xs >>= classify |> VarEnv.adding mu' @@ `Kind k
  | `Apps (`Unk _, _) -> return None
  | `Apps (`Var _, _) -> return None
  | `Apps (`Bop (_, _, l, r), _) -> (
    classify l >>= function None -> classify r | some -> return some)
  | `Arrow _ -> return @@ Some `Arrow
  | `Const (_, c) -> return @@ Some (`Const c)
  | `For (_, q, _) -> return @@ Some (q : [`All | `Unk] :> [> `All | `Unk])
  | `Lam _ -> return @@ Some `Lam
  | `Row (_, m, _) ->
    return @@ Some (m : [`Product | `Sum] :> [> `Product | `Sum])
  | _ -> failwith "classify"

and memoing t op =
  get Solved.field >>- Solved.find_opt t >>= function
  | Some t -> return t
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
  let for_as q f n =
    let* k = kind_of_dom f
    and* x =
      Unk.from f |> Unk.Unsafe.smallest @@ fun x -> has_unk x n ||| has_unk x f
    in
    UnkEnv.adding x k
    @@ let* f_x = app_of_norm at' f (unk x) in
       let* t =
         variance_of x f_x |> UnkEnv.set_variance x >> bop_of_norm o at' n f_x
       in
       let* x', (t_opt, (k, _), _) = UnkEnv.deref x in
       let gen () =
         let* v = Var.from f |> Var.Unsafe.smallest @@ fun v -> is_free v t in
         replace_of_norm x (var v) t >>- fun t ->
         `For (at', q, `Lam (at', v, k, t))
       in
       match t_opt with
       | Some t' ->
         if o = bop_of q then
           let* t' =
             let* i =
               Var.from_unk x'
               |> Var.Unsafe.smallest @@ fun i -> Core.is_free i t'
             in
             Core.replace_of_norm x (var i) t'
             >>= Core.lam_of_norm at' i k >>= Core.mu_of_norm at'
           in
           replace_of_norm x t' t
         else gen ()
       | None -> if Unk.equal x x' then gen () else replace_of_norm x (unk x') t
  in
  let unk_as x n =
    let* x, (t_opt, (k, r), v) = UnkEnv.deref x in
    kind_of n >>= Kind.unify at' k
    >>
    let u = unk x in
    (match t_opt with
    | None -> return n
    | Some t -> bop_of_norm (if v = `Zero then `Eq else o) at' n (t :> t))
    >>= presolve_of_norm
    >>= function
    | `Unk (_, x') as u' -> (
      UnkEnv.find x' >>= function
      | _, (_, r'), _ ->
        if r < r' then UnkEnv.set x' (Some u) >> return u
        else if r' < r then UnkEnv.set x (Some u') >> return u'
        else return u)
    | t -> UnkEnv.set x (Some t) >> return u
  in
  let head l r =
    match (l, r) with
    | `Var (_, l'), `Var (_, r') when Var.equal l' r' -> return l
    | n, `Unk (_, x) | `Unk (_, x), n -> unk_as x n
    | _ -> fail @@ `Error_typ_unrelated (at', (l :> t), (r :> t))
  in
  match (to_apps l, to_apps r) with
  | `Apps ((`Mu (la, lf) as lmu), lxs), _ ->
    unfold la lf lmu lxs >>= fun l -> bop_of_norm o at' l r
  | _, `Apps ((`Mu (ra, rf) as rmu), rxs) ->
    unfold ra rf rmu rxs >>= fun r -> bop_of_norm o at' l r
  | _, `Apps (`Unk (_, x), []) -> unk_as x l
  | `Apps (`Unk (_, x), []), _ -> unk_as x r
  | `Apps (lf, lxs), `Apps (rf, rxs) when List.length lxs = List.length rxs ->
    head lf rf <*> List.map2_fr (bop_of_norm `Eq at') lxs rxs >>= fun (f, xs) ->
    apps_of_norm at' f xs
  | `Arrow (_, ld, lc), `Arrow (_, rd, rc) ->
    bop_of_norm (inv o) at' ld rd <*> bop_of_norm o at' lc rc >>- fun (d, c) ->
    `Arrow (at', d, c)
  | `Const (_, lc), `Const (_, rc) when Const.equal lc rc -> return l
  | `For (_, q, f), _ -> for_as q f r
  | _, `For (_, q, f) -> for_as q f l
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
    | _ -> return @@ bop o (at', l, r))

and subst_of_norm i v t =
  let rec subst_of_norm env =
    keep_phys_eq_fr @@ function
    | `Var (_, i) as inn ->
      VarMap.find_opt i env |> Option.value ~default:inn |> return
    | `Unk _ as t ->
      is_free i t >>- fun b -> if b then failwith "subst_of_norm" else t
    | `Mu (at, t) -> subst_of_norm env t >>= mu_of_norm at
    | `Lam (at, i, k, t) as inn ->
      let env = VarMap.remove i env in
      if VarMap.is_empty env then return inn
      else
        let* i' =
          i
          |> Var.Unsafe.smallest @@ fun i ->
             is_free i inn
             ||| VarMap.exists_fr
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
    | t -> map_eq_fr (subst_of_norm env) t
  in
  match v with
  | `Var (_, i') when Var.equal i i' -> return t
  | v -> subst_of_norm (VarMap.singleton i v) t

(* *)

let sub_counter = Profiling.Counter.register "sub"

let for_as at' (f : Core.t) o op = function
  | `All ->
    let* k = kind_of_dom f
    and* fi =
      Var.from (f :> t)
      |> Var.Unsafe.smallest @@ fun fi ->
         Core.is_free fi o ||| Core.is_free fi f
    in
    Core.app_of_norm at' f @@ var fi >>= op |> VarEnv.adding fi @@ `Kind k
  | `Unk ->
    let* k = kind_of_dom f
    and* x =
      Unk.from f
      |> Unk.Unsafe.smallest @@ fun u -> Core.has_unk u o ||| Core.has_unk u f
    in
    UnkEnv.adding x k
    @@ let* t = Core.app_of_norm at' f (unk x) in
       let* _, (t_opt, _, _) =
         Core.variance_of x t |> UnkEnv.set_variance x >> op t >> UnkEnv.deref x
       in
       let* t' =
         match t_opt with
         | None ->
           let i = Var.from_unk x |> Var.freshen in
           return (var i)
         | Some t' ->
           let* i =
             Var.from_unk x |> Var.Unsafe.smallest @@ fun i -> Core.is_free i t'
           in
           Core.replace_of_norm x (var i) t'
           >>= Core.lam_of_norm at' i k >>= Core.mu_of_norm at'
       in
       Core.update_unks_of_norm x t' t >> Core.update_unks_of_norm x t' o

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
    List.iter2_fr (eq at') lxs rxs
  | _, `Apps (`Unk _, _) | `Apps (`Unk _, _), _ ->
    bop_of_norm `Join at' (l :> t) (r :> t) >>= presolve_of_norm >>- ignore
  | `Arrow (_, ld, lc), `Arrow (_, rd, rc) -> sub at' rd ld >> sub at' lc rc
  | `Const (_, lc), `Const (_, rc) when Const.equal lc rc -> unit
  | `For (at', q, f), _ -> for_as at' f r (fun l -> sub at' l r) (dual q)
  | _, `For (at', q, f) -> for_as at' f l (fun r -> sub at' l r) q
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

let find_opt_nested_arg_mu at' f arity =
  if arity <= 0 then return None
  else
    let i = Var.fresh at' in
    let is = List.init arity @@ fun _ -> Var.fresh at' in
    app_of_norm at' f (var i) >>= fun fv ->
    apps_of_norm at' fv (List.map var is)
    >>= find_map_from_all_apps_of i @@ fun _ _ xs ->
        xs
        |> List.find_map_fr @@ function
           | `Var _ -> return None
           | t -> (
             is
             |> List.find_map_fr @@ fun i ->
                is_free i t >>- function true -> Some t | false -> None)

(* *)

let rec find_opt_non_contractive is t =
  match unapp t with
  | `Mu (_, `Lam (_, i, _, f)), xs -> (
    apps_of_norm Loc.dummy f xs >>- unapp >>= function
    | (`Var (_, i') as mu), _ when Var.equal i' i || VarSet.mem i' is ->
      return @@ Some mu
    | t, _ -> find_opt_non_contractive (VarSet.add i is) t)
  | _ -> return None

let find_opt_non_contractive_mu at' f arity =
  match f with
  | `Lam (_, i, _, f) -> (
    let xs = List.init arity @@ fun _ -> var @@ Var.fresh at' in
    apps_of_norm Loc.dummy f xs >>- unapp >>= function
    | (`Var (_, i') as mu), _ when Var.equal i' i -> return @@ Some mu
    | t, _ -> find_opt_non_contractive (VarSet.singleton i) t)
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
  | `Unk (_, x) as t -> UnkEnv.find x >>- fun (_, (k, _), _) -> (t, k)

and check expected t =
  let* t, actual = infer t in
  Kind.unify (at t) expected actual >> return t

(* *)

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
  |> Kind.UnkEnv.cloning |> UnkEnv.cloning

let is_sub_of_norm l r = as_predicate check_sub_of_norm l r
let is_equal_of_norm l r = as_predicate check_equal_of_norm l r
