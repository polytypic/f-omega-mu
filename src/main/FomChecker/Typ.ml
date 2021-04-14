open FomSource
open FomDiag
open FomAnnot
open FomBasis

(* *)

include FomAST.Typ

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

let rec infer typ : _ -> Kind.t =
  let open Reader in
  let quantifier symbol f =
    let* f_kind = infer f in
    match f_kind with
    | `Arrow (_, _, (`Star _ as c_kind)) -> return c_kind
    | _ -> Error.quantifier_kind (at typ) symbol f f_kind
  in
  match typ with
  | `Mu (at', f) -> (
    let* f_kind = infer f in
    match f_kind with
    | `Star _ -> Error.mu_kind at' f f_kind
    | `Arrow (_, d_kind, c_kind) ->
      if not (Kind.equal d_kind c_kind) then
        Error.mu_kind at' f f_kind;
      let c_arity = Kind.arity c_kind in
      find_opt_nested_arg_mu at' f c_arity
      |> Option.iter (Error.mu_nested at' typ);
      find_opt_non_contractive_mu at' f c_arity
      |> Option.iter (Error.mu_non_contractive at' typ);
      return c_kind)
  | `Const (at', c) -> return @@ Const.kind_of at' c
  | `Var (at', i) -> (
    let* i_kind_opt e = Env.find_opt i e#get_typ_env in
    match i_kind_opt with
    | None -> Error.typ_var_unbound at' i
    | Some (def, i_kind) -> Annot.Typ.use i (Id.at def) >> return i_kind)
  | `Lam (at', d, d_kind, r) ->
    Annot.Typ.def d d_kind
    >> let* r_kind e = Env.add d (d, d_kind) |> e#map_typ_env |> infer r in
       return @@ `Arrow (at', d_kind, r_kind)
  | `App (at', f, x) -> (
    let* f_kind = infer f in
    match f_kind with
    | `Star _ -> Error.app_of_kind_star at' f x
    | `Arrow (_, d_kind, c_kind) ->
      let* x_kind = infer x in
      Kind.check_equal at' x_kind d_kind;
      return c_kind)
  | `ForAll (_, f) -> quantifier FomPP.for_all f
  | `Exists (_, f) -> quantifier FomPP.exists f
  | `Arrow (at', d, c) ->
    let star = `Star at' in
    check star d >> check star c >> return star
  | `Product (at', ls) | `Sum (at', ls) ->
    let star = `Star at' in
    ls |> iter (snd >>> check star) >> return star

and check expected t =
  let open Reader in
  let* actual = infer t in
  Kind.check_equal (at t) expected actual;
  return ()

(* *)

let rec kind_of checked_typ : _ -> Kind.t =
  let open Reader in
  match checked_typ with
  | `Mu (_, f) -> kind_of_cod f
  | `Const (at', c) -> return @@ Const.kind_of at' c
  | `Var (_, i) -> (
    let* i_kind_opt e = Env.find_opt i e#get_typ_env in
    match i_kind_opt with
    | None -> failwith "Impossible"
    | Some (_, i_kind) -> return i_kind)
  | `Lam (at', d, d_kind, r) ->
    let* r_kind e = Env.add d (d, d_kind) |> e#map_typ_env |> kind_of r in
    return @@ `Arrow (at', d_kind, r_kind)
  | `App (_, f, _) -> kind_of_cod f
  | `ForAll (at', _)
  | `Exists (at', _)
  | `Arrow (at', _, _)
  | `Product (at', _)
  | `Sum (at', _) ->
    return @@ `Star at'

and kind_of_cod checked_typ : _ -> Kind.t =
  let open Reader in
  let* f_kind = kind_of checked_typ in
  match f_kind with
  | `Star _ -> failwith "Impossible"
  | `Arrow (_, _, c_kind) -> return c_kind

(* *)

let unfold at f mu xs = FomAST.Typ.app at (`App (at, f, mu)) xs |> norm

let rec unfold_of_norm typ =
  match unapp typ with
  | (`Mu (at', f) as mu), xs -> unfold_of_norm (unfold at' f mu xs)
  | _ -> typ

module Goal = struct
  include Compare.Pair (FomAST.Typ) (FomAST.Typ)

  let map f = Pair.map f f

  let free_vars_to_regular_assoc (lhs, rhs) =
    IdSet.union (free lhs) (free rhs)
    |> IdSet.elements
    |> List.mapi (fun i v ->
           (v, Id.of_string (Id.at v) ("#" ^ string_of_int i)))

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
          | [], _ -> ()
          | (ll, _) :: _, [] -> Error.label_missing at ll l r
          | ((ll, lt) :: ls as lls), (ml, mt) :: ms ->
            let c = Label.compare ll ml in
            if c = 0 then (
              op (mt, lt);
              subset op ls ms)
            else if 0 < c then
              subset op lls ms
            else
              Error.label_missing at ll l r
        in
        match g with
        | `Arrow (_, ld, lc), `Arrow (_, rd, rc) ->
          sub (rd, ld);
          sub (lc, rc)
        | `Product (_, lls), `Product (_, rls) -> subset sub rls lls
        | `Sum (_, lls), `Sum (_, rls) -> subset (Pair.swap >>> sub) lls rls
        | `ForAll (_, l), `ForAll (_, r) | `Exists (_, l), `Exists (_, r) ->
          sub (l, r)
        | `Lam (_, li, lk, lt), `Lam (_, ri, rk, rt) ->
          Kind.check_equal at lk rk;
          sub (Goal.unify_vars li lt ri rt |> snd |> Goal.regularize_free_vars)
        | _ -> (
          match (unapp l, unapp r) with
          | ((`Mu (la, lf) as lmu), lxs), ((`Mu (ra, rf) as rmu), rxs) ->
            sub (unfold la lf lmu lxs, unfold ra rf rmu rxs)
          | ((`Mu (la, lf) as lmu), lxs), _ -> sub (unfold la lf lmu lxs, r)
          | _, ((`Mu (ra, rf) as rmu), rxs) -> sub (l, unfold ra rf rmu rxs)
          | (lf, lx :: lxs), (rf, rx :: rxs) ->
            if List.length lxs <> List.length rxs then
              Error.typ_mismatch at r l;
            eq (lf, rf);
            eq (lx, rx);
            List.iter2 (fun l r -> eq (l, r)) lxs rxs
          | _ -> Error.typ_mismatch at r l))
    and eq g =
      sub g;
      sub (Pair.swap g)
    in
    (sub, eq)
  in
  let sub at g = g |> Goal.regularize_free_vars |> fst (make_sub_and_eq at) in
  let eq at g = g |> Goal.regularize_free_vars |> snd (make_sub_and_eq at) in
  (sub, eq)

let is_sub_of_norm, is_equal_of_norm =
  let as_predicate check g =
    try
      check Loc.dummy g;
      true
    with Diagnostic.Error _ -> false
  in
  (as_predicate check_sub_of_norm, as_predicate check_equal_of_norm)

(* *)

let rec to_strict (t : ['a FomAST.Typ.f | `Lazy of 'a Lazy.t] as 'a) =
  match t with
  | `Mu (at, t) -> `Mu (at, to_strict t)
  | `Const _ as inn -> inn
  | `Var _ as inn -> inn
  | `Lam (at, i, k, t) -> `Lam (at, i, k, to_strict t)
  | `App (at, f, x) -> `App (at, to_strict f, to_strict x)
  | `ForAll (at, t) -> `ForAll (at, to_strict t)
  | `Exists (at, t) -> `Exists (at, to_strict t)
  | `Arrow (at, d, c) -> `Arrow (at, to_strict d, to_strict c)
  | `Product (at, ls) ->
    `Product (at, ls |> List.map (Pair.map Fun.id to_strict))
  | `Sum (at, ls) -> `Sum (at, ls |> List.map (Pair.map Fun.id to_strict))
  | `Lazy (lazy t) -> to_strict t

let rec to_lazy = function
  | `Mu (at, t) -> `Mu (at, to_lazy t)
  | `Const _ as inn -> inn
  | `Var _ as inn -> inn
  | `Lam (at, i, k, t) -> `Lam (at, i, k, to_lazy t)
  | `App (at, f, x) -> `App (at, to_lazy f, to_lazy x)
  | `ForAll (at, t) -> `ForAll (at, to_lazy t)
  | `Exists (at, t) -> `Exists (at, to_lazy t)
  | `Arrow (at, d, c) -> `Arrow (at, to_lazy d, to_lazy c)
  | `Product (at, ls) -> `Product (at, ls |> List.map (Pair.map Fun.id to_lazy))
  | `Sum (at, ls) -> `Sum (at, ls |> List.map (Pair.map Fun.id to_lazy))

let join_of_norm at g =
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
        intersection op ((ll, op (lt, rt)) :: os) (lls, rls)
    | [], _ | _, [] -> List.rev os
  in
  let rec union op os = function
    | ((ll, lt) :: lls as llls), ((rl, rt) :: rls as rlls) ->
      let c = Label.compare ll rl in
      if c < 0 then
        union op ((ll, to_lazy lt) :: os) (lls, rlls)
      else if 0 < c then
        union op ((rl, to_lazy rt) :: os) (llls, rls)
      else
        union op ((ll, op (lt, rt)) :: os) (lls, rls)
    | (ll, lt) :: lls, [] -> union op ((ll, to_lazy lt) :: os) (lls, [])
    | [], (rl, rt) :: rls -> union op ((rl, to_lazy rt) :: os) ([], rls)
    | [], [] -> List.rev os
  in
  let synth map fst snd upper lower intersection union ((l, r) as g) =
    match GoalMap.find_opt g !map with
    | Some result -> result
    | None ->
      let result =
        if is_sub_of_norm g then
          to_lazy (snd g)
        else if is_sub_of_norm (Pair.swap g) then
          to_lazy (fst g)
        else
          `Lazy
            (lazy
              (match g with
              | `Arrow (_, ld, lc), `Arrow (_, rd, rc) ->
                `Arrow (at, lower (ld, rd), upper (lc, rc))
              | `Product (_, lls), `Product (_, rls) ->
                `Product (at, intersection upper [] (lls, rls))
              | `Sum (_, lls), `Sum (_, rls) ->
                `Sum (at, union upper [] (lls, rls))
              | `Lam (_, li, lk, lt), `Lam (_, ri, rk, rt) ->
                Kind.check_equal at lk rk;
                let i, goal = Goal.unify_vars li lt ri rt in
                let assoc = Goal.free_vars_to_regular_assoc goal in
                let t =
                  goal
                  |> Goal.map (Goal.to_subst assoc)
                  |> upper
                  |> (to_strict
                     >>> Goal.to_subst (List.map Pair.swap assoc)
                     >>> to_lazy)
                in
                `Lam (at, i, lk, t)
              | `ForAll (_, lt), `ForAll (_, rt) -> `ForAll (at, upper (lt, rt))
              | `Exists (_, lt), `Exists (_, rt) -> `Exists (at, upper (lt, rt))
              | _ -> (
                match (unapp l, unapp r) with
                | ((`Mu (la, lf) as lmu), lxs), ((`Mu (ra, rf) as rmu), rxs) ->
                  upper (unfold la lf lmu lxs, unfold ra rf rmu rxs)
                | ((`Mu (la, lf) as lmu), lxs), _ ->
                  upper (unfold la lf lmu lxs, r)
                | _, ((`Mu (ra, rf) as rmu), rxs) ->
                  upper (l, unfold ra rf rmu rxs)
                | _ -> Error.typ_mismatch at l r)))
      in
      map := GoalMap.add g result !map;
      result
  in
  let rec join g = synth joins fst snd join meet intersection union g
  and meet g = synth meets snd fst meet join union intersection g in
  join g |> to_strict
