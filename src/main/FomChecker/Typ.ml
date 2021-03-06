open FomSource
open FomAnnot
open FomBasis

(* *)

open Rea

(* *)

include FomAST.Typ

(* *)

module Env = struct
  include Env

  type t = (Id.t * Kind.t) Env.t

  let empty = initial_env
  let field r = r#typ_env
  let adding i k = mapping field @@ Env.add i (i, k)
  let find_opt i = get_as field @@ Env.find_opt i
  let resetting op = setting field empty op

  class con =
    object
      val typ_env : t = empty
      method typ_env = Field.make typ_env (fun v -> {<typ_env = v>})
    end
end

(* *)

let rec find_map_from_all_apps_of i' p = function
  | `Const _ -> None
  | `Lam (_, i, _, t) ->
    if Id.equal i i' then None else find_map_from_all_apps_of i' p t
  | `Mu (_, t) | `ForAll (_, t) | `Exists (_, t) ->
    find_map_from_all_apps_of i' p t
  | `Arrow (_, d, c) -> (
    match find_map_from_all_apps_of i' p d with
    | None -> find_map_from_all_apps_of i' p c
    | some -> some)
  | `Product (_, ls) | `Sum (_, ls) ->
    ls |> List.find_map (snd >>> find_map_from_all_apps_of i' p)
  | (`App _ | `Var _) as t -> (
    match unapp t with
    | (`Var (_, i) as f), xs ->
      if Id.equal i i' then
        match p t f xs with
        | None -> xs |> List.find_map (find_map_from_all_apps_of i' p)
        | some -> some
      else
        xs |> List.find_map (find_map_from_all_apps_of i' p)
    | f, xs -> (
      match find_map_from_all_apps_of i' p f with
      | None -> xs |> List.find_map (find_map_from_all_apps_of i' p)
      | some -> some))

let find_opt_nested_arg_mu at f arity =
  if arity <= 0 then
    None
  else
    let i = Id.fresh at in
    let v = `Var (at, i) in
    let is = List.init arity (fun _ -> Id.fresh at) in
    let vs = is |> List.map (fun i -> `Var (at, i)) in
    app at (`App (at, f, v)) vs
    |> norm
    |> find_map_from_all_apps_of i @@ fun _ _ xs ->
       xs
       |> List.find_map @@ function
          | `Var _ -> None
          | t ->
            is |> List.find_map @@ fun i -> if is_free i t then Some t else None

let find_opt_nested_arg t =
  match unapp t with
  | `Mu (at, f), xs -> find_opt_nested_arg_mu at f (List.length xs)
  | _ -> None

let rec find_opt_non_contractive ids typ =
  match unapp typ with
  | `Mu (_, `Lam (_, id, _, f)), xs -> (
    match app Loc.dummy f xs |> norm |> unapp with
    | (`Var (_, id') as mu), _ when Id.equal id' id || IdSet.mem id' ids ->
      Some mu
    | typ, _ -> find_opt_non_contractive (IdSet.add id ids) typ)
  | _ -> None

let find_opt_non_contractive_mu at f arity =
  match f with
  | `Lam (_, id, _, f) -> (
    let is = List.init arity (fun _ -> Id.fresh at) in
    let xs = is |> List.map (fun i -> `Var (at, i)) in
    match app Loc.dummy f xs |> norm |> unapp with
    | (`Var (_, id') as mu), _ when Id.equal id' id -> Some mu
    | typ, _ -> find_opt_non_contractive (IdSet.singleton id) typ)
  | _ -> None

(* *)

let rec resolve = function
  | `Mu (at', f) as t ->
    let+ f' = resolve f in
    if f == f' then t else `Mu (at', f')
  | `Const (_, _) as t -> return t
  | `Var (_, _) as t -> return t
  | `Lam (at', d, d_kind, r) as t ->
    let+ d_kind' = Kind.resolve d_kind and+ r' = resolve r in
    if d_kind == d_kind' && r == r' then t else `Lam (at', d, d_kind', r')
  | `App (at', f, x) as t ->
    let+ f' = resolve f and+ x' = resolve x in
    if f == f' && x = x' then t else `App (at', f', x')
  | `ForAll (at', f) as t ->
    let+ f' = resolve f in
    if f == f' then t else `ForAll (at', f')
  | `Exists (at', f) as t ->
    let+ f' = resolve f in
    if f == f' then t else `Exists (at', f')
  | `Arrow (at', d, c) as t ->
    let+ d' = resolve d and+ c' = resolve c in
    if d == d' && c == c' then t else `Arrow (at', d', c')
  | `Product (at', ls) as t ->
    let+ ls' =
      ls |> MList.traverse_phys_eq @@ MPair.traverse_phys_eq return resolve
    in
    if ls == ls' then t else `Product (at', ls')
  | `Sum (at', ls) as t ->
    let+ ls' =
      ls |> MList.traverse_phys_eq @@ MPair.traverse_phys_eq return resolve
    in
    if ls == ls' then t else `Sum (at', ls')

(* *)

let rec ground = function
  | `Mu (at', f) as t ->
    let f' = ground f in
    if f == f' then t else `Mu (at', f')
  | `Const (_, _) as t -> t
  | `Var (_, _) as t -> t
  | `Lam (at', d, d_kind, r) as t ->
    let d_kind' = Kind.ground d_kind and r' = ground r in
    if d_kind == d_kind' && r == r' then t else `Lam (at', d, d_kind', r')
  | `App (at', f, x) as t ->
    let f' = ground f and x' = ground x in
    if f == f' && x = x' then t else `App (at', f', x')
  | `ForAll (at', f) as t ->
    let f' = ground f in
    if f == f' then t else `ForAll (at', f')
  | `Exists (at', f) as t ->
    let f' = ground f in
    if f == f' then t else `Exists (at', f')
  | `Arrow (at', d, c) as t ->
    let d' = ground d and c' = ground c in
    if d == d' && c == c' then t else `Arrow (at', d', c')
  | `Product (at', ls) as t ->
    let ls' = ls |> ListExt.map_phys_eq @@ Pair.map_phys_eq Fun.id ground in
    if ls == ls' then t else `Product (at', ls')
  | `Sum (at', ls) as t ->
    let ls' = ls |> ListExt.map_phys_eq @@ Pair.map_phys_eq Fun.id ground in
    if ls == ls' then t else `Sum (at', ls')

(* *)

let rec infer = function
  | `Mu (at', f) as typ ->
    let* f_kind = infer f in
    let kind = `Var (at', Kind.Id.fresh at') in
    Kind.unify at' (`Arrow (at', kind, kind)) f_kind
    >> let* arity = Kind.resolve kind >>- Kind.min_arity in
       find_opt_nested_arg_mu at' f arity
       |> MOption.iter (fun typ' ->
              let* typ = resolve typ and* typ' = resolve typ' in
              fail @@ `Error_mu_nested (at', typ, typ'))
       >> (find_opt_non_contractive_mu at' f arity
          |> MOption.iter (fun typ' ->
                 let* typ = resolve typ and* typ' = resolve typ' in
                 fail @@ `Error_mu_non_contractive (at', typ, typ')))
       >> return kind
  | `Const (at', c) -> return @@ Const.kind_of at' c
  | `Var (at', i) -> (
    let* i_kind_opt = Env.find_opt i in
    match i_kind_opt with
    | None -> fail @@ `Error_typ_var_unbound (at', i)
    | Some (def, i_kind) -> Annot.Typ.use i (Id.at def) >> return i_kind)
  | `Lam (at', d, d_kind, r) ->
    let+ r_kind = Annot.Typ.def d d_kind >> Env.adding d d_kind (infer r) in
    `Arrow (at', d_kind, r_kind)
  | `App (at', f, x) ->
    let* f_kind = infer f and* d_kind = infer x in
    let c_kind = `Var (at', Kind.Id.fresh at') in
    Kind.unify at' (`Arrow (at', d_kind, c_kind)) f_kind >> return c_kind
  | `ForAll (_, f) as typ -> infer_quantifier typ f
  | `Exists (_, f) as typ -> infer_quantifier typ f
  | `Arrow (at', d, c) ->
    let star = `Star at' in
    check star d >> check star c >> return star
  | `Product (at', ls) | `Sum (at', ls) ->
    let star = `Star at' in
    ls |> MList.iter (snd >>> check star) >> return star

and infer_quantifier t f =
  let* f_kind = infer f in
  let at' = at t in
  let d_kind = `Var (at', Kind.Id.fresh at') in
  let c_kind = `Star at' in
  Kind.unify at' (`Arrow (at', d_kind, c_kind)) f_kind >> return c_kind

and check expected t =
  let* actual = infer t in
  Kind.unify (at t) expected actual

(* *)

let infer_and_resolve t =
  let* _ = infer t in
  resolve t

(* *)

let rec kind_of = function
  | `Mu (_, f) -> kind_of_cod f
  | `Const (at', c) -> return @@ Const.kind_of at' c
  | `Var (_, i) -> (
    let+ i_kind_opt = Env.find_opt i in
    match i_kind_opt with
    | None -> failwith "impossible"
    | Some (_, i_kind) -> i_kind)
  | `Lam (at', d, d_kind, r) ->
    let+ r_kind = Env.adding d d_kind (kind_of r) in
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
  | `Star _ | `Var (_, _) -> failwith "impossible"
  | `Arrow (_, _, c_kind) -> c_kind

let kind_of t = kind_of t >>= Kind.resolve

(* *)

let unfold at f mu xs = FomAST.Typ.app at (`App (at, f, mu)) xs |> norm

let rec unfold_of_norm typ =
  match unapp typ with
  | (`Mu (at', f) as mu), xs -> unfold_of_norm (unfold at' f mu xs)
  | _ -> typ

(* *)

module Goal = struct
  include Compare.Pair (FomAST.Typ) (FomAST.Typ)

  let map f = Pair.map f f

  let free_vars_to_regular_assoc (lhs, rhs) =
    IdSet.union (free lhs) (free rhs)
    |> IdSet.elements
    |> List.mapi @@ fun i v ->
       (v, Id.of_string (Id.at v) ("#" ^ string_of_int i))

  let to_subst =
    List.to_seq
    >>> Seq.map (Pair.map Fun.id @@ fun i -> `Var (Id.at i, i))
    >>> Env.of_seq >>> subst_par

  let regularize_free_vars goal =
    map (goal |> free_vars_to_regular_assoc |> to_subst) goal

  let unify_vars li lt ri rt =
    if Id.equal li ri then
      (li, (lt, rt))
    else
      let i = Id.fresh Loc.dummy in
      let v = `Var (Loc.dummy, i) in
      (i, (subst li v lt, subst ri v rt))
end

(* *)

let check_sub_of_norm, check_equal_of_norm =
  let make_sub_and_eq at =
    let module GoalSet = Set.Make (Goal) in
    let goals = ref GoalSet.empty in
    let rec sub ((l, r) as g) =
      if 0 <> FomAST.Typ.compare l r && not (GoalSet.mem g !goals) then (
        goals := GoalSet.add g !goals;
        let rec subset op ls ms =
          match (ls, ms) with
          | [], _ -> unit
          | (ll, _) :: _, [] -> fail @@ `Error_label_missing (at, ll, l, r)
          | ((ll, lt) :: ls as lls), (ml, mt) :: ms ->
            let c = Label.compare ll ml in
            if c = 0 then
              op (mt, lt) >> subset op ls ms
            else if 0 < c then
              subset op lls ms
            else
              fail @@ `Error_label_missing (at, ll, l, r)
        in
        match g with
        | `Arrow (_, ld, lc), `Arrow (_, rd, rc) -> sub (rd, ld) >> sub (lc, rc)
        | `Product (_, lls), `Product (_, rls) -> subset sub rls lls
        | `Sum (_, lls), `Sum (_, rls) -> subset (Pair.swap >>> sub) lls rls
        | `ForAll (_, l), `ForAll (_, r) | `Exists (_, l), `Exists (_, r) ->
          sub (l, r)
        | `Lam (_, li, lk, lt), `Lam (_, ri, rk, rt) ->
          Kind.unify at lk rk
          >> sub
               (Goal.unify_vars li lt ri rt |> snd |> Goal.regularize_free_vars)
        | _ -> (
          match (unapp l, unapp r) with
          | ((`Mu (la, lf) as lmu), lxs), ((`Mu (ra, rf) as rmu), rxs) ->
            sub (unfold la lf lmu lxs, unfold ra rf rmu rxs)
          | ((`Mu (la, lf) as lmu), lxs), _ -> sub (unfold la lf lmu lxs, r)
          | _, ((`Mu (ra, rf) as rmu), rxs) -> sub (l, unfold ra rf rmu rxs)
          | (lf, lx :: lxs), (rf, rx :: rxs) ->
            if List.length lxs <> List.length rxs then
              fail @@ `Error_typ_mismatch (at, r, l)
            else
              eq (lf, rf)
              >> eq (lx, rx)
              >> MList.iter2 (fun l r -> eq (l, r)) lxs rxs
          | _ -> fail @@ `Error_typ_mismatch (at, r, l)))
      else
        unit
    and eq g = sub g >> sub (Pair.swap g) in
    (sub, eq)
  in
  let sub at g = g |> Goal.regularize_free_vars |> fst (make_sub_and_eq at) in
  let eq at g = g |> Goal.regularize_free_vars |> snd (make_sub_and_eq at) in
  (sub, eq)

let is_sub_of_norm, is_equal_of_norm =
  let as_predicate check g =
    check Loc.dummy g
    |> try_in (Fun.const @@ return true) (Fun.const @@ return false)
  in
  let sub g = as_predicate check_sub_of_norm g in
  let eq g = as_predicate check_equal_of_norm g in
  (sub, eq)

(* *)

module TypSet = Set.Make (FomAST.Typ)

let rec contract t =
  let* s, t = contract_base t in
  let+ t_opt =
    s |> TypSet.elements |> MList.find_opt (fun mu -> is_equal_of_norm (t, mu))
  in
  let s, u =
    match t_opt with
    | Some t -> (s, t)
    | None -> (
      match unapp t with `Mu _, _ -> (TypSet.add t s, t) | _ -> (s, t))
  in
  match t with
  | `Lam (_, i, _, _) -> (s |> TypSet.filter (not <<< is_free i), u)
  | _ -> (s, u)

and contract_base = function
  | `Mu (at', e) as t ->
    let+ s, e' = contract e in
    (s, if e == e' then t else `Mu (at', e'))
  | (`Const (_, _) | `Var (_, _)) as t -> return (TypSet.empty, t)
  | `Lam (at', x, k, e) as t ->
    let+ s, e' = contract e in
    (s, if e == e' then t else `Lam (at', x, k, e'))
  | `App (at', f, x) as t ->
    let+ fs, f' = contract f and+ xs, x' = contract x in
    let s = TypSet.union fs xs in
    (s, if f == f' && x == x' then t else `App (at', f', x'))
  | `ForAll (at', e) as t ->
    let+ s, e' = contract e in
    (s, if e == e' then t else `ForAll (at', e'))
  | `Exists (at', e) as t ->
    let+ s, e' = contract e in
    (s, if e == e' then t else `Exists (at', e'))
  | `Arrow (at', d, c) as t ->
    let+ ds, d' = contract d and+ cs, c' = contract c in
    (TypSet.union ds cs, if d == d' && c == c' then t else `Arrow (at', d', c'))
  | `Product (at', ls) as t ->
    let+ s, ls' = contract_labels ls in
    (s, if ls == ls' then t else `Product (at', ls'))
  | `Sum (at', ls) as t ->
    let+ s, ls' = contract_labels ls in
    (s, if ls == ls' then t else `Sum (at', ls'))

and contract_labels ls =
  let+ sls' = ls |> MList.traverse @@ MPair.traverse return contract in
  let ls' =
    sls'
    |> List.map (fun (l, (_, t)) -> (l, t))
    |> ListExt.share_phys_eq
         (Pair.share_phys_eq (fun _ x -> x) (fun _ x -> x))
         ls
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
    let mus, e_vs = collect_mus_closed (IdSet.add i bvs) e mus in
    let e_vs = IdSet.remove i e_vs in
    let mus = if IdSet.disjoint bvs e_vs then TypSet.add t mus else mus in
    (mus, e_vs)
  | `Mu (_, e) -> collect_mus_closed bvs e mus
  | `Const _ -> (mus, IdSet.empty)
  | `Var (_, i) -> (mus, IdSet.singleton i)
  | `App (_, f, x) ->
    let mus, f_vs = collect_mus_closed bvs f mus in
    let mus, x_vs = collect_mus_closed bvs x mus in
    (mus, IdSet.union f_vs x_vs)
  | `Lam (_, i, _, e) ->
    let mus, vs = collect_mus_closed (IdSet.add i bvs) e mus in
    (mus, IdSet.remove i vs)
  | `ForAll (_, e) | `Exists (_, e) -> collect_mus_closed bvs e mus
  | `Arrow (_, d, c) ->
    let mus, d_vs = collect_mus_closed bvs d mus in
    let mus, c_vs = collect_mus_closed bvs c mus in
    (mus, IdSet.union d_vs c_vs)
  | `Product (_, ls) | `Sum (_, ls) ->
    ls
    |> List.fold_left
         (fun (mus, vs) (_, t) ->
           let mus, t_vs = collect_mus_closed bvs t mus in
           (mus, IdSet.union vs t_vs))
         (mus, IdSet.empty)

let rec replace_closed_mus m = function
  | `Mu (at'', `Lam (at', i, k, e)) as t ->
    if TypSet.mem t m then
      `Var (at', i)
    else
      let e' = replace_closed_mus m e in
      if e == e' then t else `Mu (at'', `Lam (at', i, k, e'))
  | `Mu (at', e) as t ->
    let e' = replace_closed_mus m e in
    if e == e' then t else `Mu (at', e')
  | (`Const (_, _) | `Var (_, _)) as t -> t
  | `App (at', f, x) as t ->
    let f' = replace_closed_mus m f and x' = replace_closed_mus m x in
    if f == f' && x == x' then t else `App (at', f', x')
  | `Lam (at', i, k, e) as t ->
    let e' = replace_closed_mus m e in
    if e == e' then t else `Lam (at', i, k, e')
  | `ForAll (at', e) as t ->
    let e' = replace_closed_mus m e in
    if e == e' then t else `ForAll (at', e')
  | `Exists (at', e) as t ->
    let e' = replace_closed_mus m e in
    if e == e' then t else `Exists (at', e')
  | `Arrow (at', d, c) as t ->
    let d' = replace_closed_mus m d and c' = replace_closed_mus m c in
    if d == d' && c == c' then t else `Arrow (at', d', c')
  | `Product (at', ls) as t ->
    let ls' =
      ls |> ListExt.map_phys_eq (Pair.map_phys_eq Fun.id (replace_closed_mus m))
    in
    if ls == ls' then t else `Product (at', ls')
  | `Sum (at', ls) as t ->
    let ls' =
      ls |> ListExt.map_phys_eq (Pair.map_phys_eq Fun.id (replace_closed_mus m))
    in
    if ls == ls' then t else `Sum (at', ls')

(* *)

let rec to_strict
    (t : [('a, 'k) FomAST.Typ.f | `Lazy of ('r, 'e, 'a) Rea.t Lazy.t] as 'a) =
  match t with
  | `Mu (at, t) ->
    let+ t = to_strict t in
    `Mu (at, t)
  | `Const _ as inn -> return inn
  | `Var _ as inn -> return inn
  | `Lam (at, i, k, t) ->
    let+ t = to_strict t in
    `Lam (at, i, k, t)
  | `App (at, f, x) ->
    let+ f = to_strict f and+ x = to_strict x in
    `App (at, f, x)
  | `ForAll (at, t) ->
    let+ t = to_strict t in
    `ForAll (at, t)
  | `Exists (at, t) ->
    let+ t = to_strict t in
    `Exists (at, t)
  | `Arrow (at, d, c) ->
    let+ d = to_strict d and+ c = to_strict c in
    `Arrow (at, d, c)
  | `Product (at, ls) ->
    let+ ls = ls |> MList.traverse @@ MPair.traverse return to_strict in
    `Product (at, ls)
  | `Sum (at, ls) ->
    let+ ls = ls |> MList.traverse @@ MPair.traverse return to_strict in
    `Sum (at, ls)
  | `Lazy (lazy t) ->
    let* t = t in
    to_strict t

let to_lazy e =
  (e
    : [ | ('a, 'k) FomAST.Typ.f] as 'a
    :> [('b, 'k) FomAST.Typ.f | `Lazy of ('r, 'e, 'b) Rea.t Lazy.t] as 'b)

let join_of_norm, meet_of_norm =
  let make_join_and_meet at =
    let module GoalMap = Map.Make (Goal) in
    let joins = ref GoalMap.empty in
    let meets = ref GoalMap.empty in
    let rec intersection op os = function
      | ((ll, lt) :: lls as llls), ((rl, rt) :: rls as rlls) ->
        let c = Label.compare ll rl in
        if c < 0 then
          intersection op os (lls, rlls)
        else if 0 < c then
          intersection op os (llls, rls)
        else
          let* t = op (lt, rt) in
          intersection op ((ll, t) :: os) (lls, rls)
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
          let* t = op (lt, rt) in
          union op ((ll, t) :: os) (lls, rls)
      | (ll, lt) :: lls, [] -> union op ((ll, to_lazy lt) :: os) (lls, [])
      | [], (rl, rt) :: rls -> union op ((rl, to_lazy rt) :: os) ([], rls)
      | [], [] -> return @@ List.rev os
    in
    let synth map fst snd upper lower intersection union ((l, r) as g) =
      match GoalMap.find_opt g !map with
      | Some result -> result
      | None ->
        let result =
          let* g_is_sub = is_sub_of_norm g in
          if g_is_sub then
            return @@ to_lazy (snd g)
          else
            let* swap_g_is_sub = is_sub_of_norm (Pair.swap g) in
            if swap_g_is_sub then
              return @@ to_lazy (fst g)
            else
              return
              @@ `Lazy
                   (lazy
                     (match g with
                     | `Arrow (_, ld, lc), `Arrow (_, rd, rc) ->
                       let+ d = lower (ld, rd) and+ c = upper (lc, rc) in
                       `Arrow (at, d, c)
                     | `Product (_, lls), `Product (_, rls) ->
                       let+ ls = intersection upper [] (lls, rls) in
                       `Product (at, ls)
                     | `Sum (_, lls), `Sum (_, rls) ->
                       let+ ls = union upper [] (lls, rls) in
                       `Sum (at, ls)
                     | `Lam (_, li, lk, lt), `Lam (_, ri, rk, rt) ->
                       Kind.unify at lk rk
                       >>
                       let i, goal = Goal.unify_vars li lt ri rt in
                       let assoc = Goal.free_vars_to_regular_assoc goal in
                       let+ t =
                         goal
                         |> Goal.map (Goal.to_subst assoc)
                         |> upper >>= to_strict
                         >>- Goal.to_subst (List.map Pair.swap assoc)
                         >>- to_lazy
                       in
                       `Lam (at, i, lk, t)
                     | `ForAll (_, lt), `ForAll (_, rt) ->
                       let+ t = upper (lt, rt) in
                       `ForAll (at, t)
                     | `Exists (_, lt), `Exists (_, rt) ->
                       let+ t = upper (lt, rt) in
                       `Exists (at, t)
                     | _ -> (
                       match (unapp l, unapp r) with
                       | ( ((`Mu (la, lf) as lmu), lxs),
                           ((`Mu (ra, rf) as rmu), rxs) ) ->
                         upper (unfold la lf lmu lxs, unfold ra rf rmu rxs)
                       | ((`Mu (la, lf) as lmu), lxs), _ ->
                         upper (unfold la lf lmu lxs, r)
                       | _, ((`Mu (ra, rf) as rmu), rxs) ->
                         upper (l, unfold ra rf rmu rxs)
                       | _ -> fail @@ `Error_typ_mismatch (at, l, r))))
        in
        map := GoalMap.add g result !map;
        result
    in
    let rec join g = synth joins fst snd join meet intersection union g
    and meet g = synth meets snd fst meet join union intersection g in
    (join, meet)
  in
  let join at g = fst (make_join_and_meet at) g >>= to_strict
  and meet at g = snd (make_join_and_meet at) g >>= to_strict in
  (join, meet)
