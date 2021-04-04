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
    if Id.equal i i' then
      None
    else
      find_map_from_all_apps_of i' p t
  | `Mu (_, t) | `ForAll (_, t) | `Exists (_, t) ->
    find_map_from_all_apps_of i' p t
  | `Arrow (_, d, c) -> (
    match find_map_from_all_apps_of i' p d with
    | None -> find_map_from_all_apps_of i' p c
    | some -> some)
  | `Product (_, ls) | `Sum (_, ls) ->
    ls |> List.find_map (snd >> find_map_from_all_apps_of i' p)
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
            is
            |> List.find_map @@ fun i ->
               if is_free i t then
                 Some t
               else
                 None

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
  | `Const (at', c) -> return (Const.kind_of at' c)
  | `Var (at', i) -> (
    let* i_kind_opt e = Env.find_opt i e#get_typ_env in
    match i_kind_opt with
    | None -> Error.typ_var_unbound at' i
    | Some (def, i_kind) ->
      let* () = Annot.Typ.use i (Id.at def) in
      return i_kind)
  | `Lam (at', d, d_kind, r) ->
    let* () = Annot.Typ.def d d_kind in
    let* r_kind e = Env.add d (d, d_kind) |> e#map_typ_env |> infer r in
    return (`Arrow (at', d_kind, r_kind))
  | `App (at', f, x) -> (
    let* f_kind = infer f in
    match f_kind with
    | `Star _ -> Error.app_of_kind_star at' f x
    | `Arrow (_, d_kind, c_kind) ->
      let* x_kind = infer x in
      if not (Kind.equal d_kind x_kind) then
        Error.app_kind_mismatch at' f d_kind x x_kind;
      return c_kind)
  | `ForAll (_, f) -> quantifier FomPP.for_all f
  | `Exists (_, f) -> quantifier FomPP.exists f
  | `Arrow (at', d, c) ->
    let star = `Star at' in
    let* () = check star d in
    let* () = check star c in
    return star
  | `Product (at', ls) | `Sum (at', ls) ->
    let star = `Star at' in
    let* () = ls |> iter (snd >> check star) in
    return star

and check expected t =
  let open Reader in
  let* actual = infer t in
  if not (Kind.equal expected actual) then
    Error.kind_mismatch (at t) expected actual;
  return ()

let rec kind_of checked_typ : _ -> Kind.t =
  let open Reader in
  match checked_typ with
  | `Mu (_, f) -> kind_of_cod f
  | `Const (at', c) -> return (Const.kind_of at' c)
  | `Var (_, i) -> (
    let* i_kind_opt e = Env.find_opt i e#get_typ_env in
    match i_kind_opt with
    | None -> failwith "Impossible"
    | Some (_, i_kind) -> return i_kind)
  | `Lam (at', d, d_kind, r) ->
    let* r_kind e = Env.add d (d, d_kind) |> e#map_typ_env |> kind_of r in
    return (`Arrow (at', d_kind, r_kind))
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

let unfold at f mu xs = FomAST.Typ.app at (`App (at, f, mu)) xs |> norm

let rec unfold_of_norm typ =
  match unapp typ with
  | (`Mu (at', f) as mu), xs -> unfold_of_norm (unfold at' f mu xs)
  | _ -> typ

module Goal = struct
  include Compare.Pair (FomAST.Typ) (FomAST.Typ)

  let map f = Pair.map f f
end

module GoalSet = struct
  include Set.Make (Goal)

  let is_trivial (lhs, rhs) = 0 = FomAST.Typ.compare lhs rhs

  (* *)
  let singleton goal = if is_trivial goal then empty else singleton goal

  let singleton_eq goal =
    if is_trivial goal then empty else of_list [goal; Pair.swap goal]

  (* *)
  let add goal goals = if is_trivial goal then goals else add goal goals
  let add_inv = Pair.swap >> add

  (* *)
  let of_list_eq goals =
    let goals = List.filter (is_trivial >> not) goals in
    goals
    |> List.fold_left (fun goals goal -> Pair.swap goal :: goals) goals
    |> of_list

  let of_list = List.filter (is_trivial >> not) >> of_list
end

let free_vars_to_regular_assoc (lhs, rhs) =
  IdSet.union (free lhs) (free rhs)
  |> IdSet.elements
  |> List.mapi (fun i v -> (v, Id.of_string (Id.at v) ("#" ^ string_of_int i)))

let to_subst =
  List.to_seq
  >> Seq.map (Pair.map id @@ fun i -> `Var (Id.at i, i))
  >> Env.of_seq >> subst_par

let regularize_free_vars goal =
  Goal.map (goal |> free_vars_to_regular_assoc |> to_subst) goal

let intersect_labels add ls ms =
  let rec loop goals ls ms =
    match (ls, ms) with
    | [], _ -> Some goals
    | ((ll, lt) :: ls as lls), (ml, mt) :: ms ->
      if Label.equal ll ml then
        loop (add (mt, lt) goals) ls ms
      else
        loop goals lls ms
    | _ :: _, [] -> None
  in
  loop GoalSet.empty ls ms

let unify_vars lhs_i lhs_t rhs_i rhs_t =
  if Id.equal lhs_i rhs_i then
    (lhs_i, (lhs_t, rhs_t))
  else
    let i = Id.fresh Loc.dummy in
    let v = `Var (Loc.dummy, i) in
    (i, (subst lhs_i v lhs_t, subst rhs_i v rhs_t))

let support (lhs, rhs) =
  match (lhs, rhs) with
  | `Arrow (_, lhs_d, lhs_c), `Arrow (_, rhs_d, rhs_c) ->
    Some ([(rhs_d, lhs_d); (lhs_c, rhs_c)] |> GoalSet.of_list)
  | `Product (_, lhs_ls), `Product (_, rhs_ls) ->
    intersect_labels GoalSet.add rhs_ls lhs_ls
  | `Sum (_, lhs_ls), `Sum (_, rhs_ls) ->
    intersect_labels GoalSet.add_inv lhs_ls rhs_ls
  | `ForAll (_, lhs), `ForAll (_, rhs) | `Exists (_, lhs), `Exists (_, rhs) ->
    Some (GoalSet.singleton (lhs, rhs))
  | `Lam (_, lhs_id, lhs_kind, lhs_typ), `Lam (_, rhs_id, rhs_kind, rhs_typ) ->
    if Kind.equal lhs_kind rhs_kind then
      let _, goal = unify_vars lhs_id lhs_typ rhs_id rhs_typ in
      Some (goal |> regularize_free_vars |> GoalSet.singleton)
    else
      None
  | _ -> (
    match (unapp lhs, unapp rhs) with
    | ((`Mu (lat, lf) as lmu), lxs), ((`Mu (rat, rf) as rmu), rxs) ->
      Some (GoalSet.singleton (unfold lat lf lmu lxs, unfold rat rf rmu rxs))
    | ((`Mu (lat, lf) as lmu), lxs), _ ->
      Some (GoalSet.singleton (unfold lat lf lmu lxs, rhs))
    | _, ((`Mu (rat, rf) as rmu), rxs) ->
      Some (GoalSet.singleton (lhs, unfold rat rf rmu rxs))
    | (lf, lx :: lxs), (rf, rx :: rxs) when List.length lxs = List.length rxs ->
      Some
        (List.combine (lf :: lx :: lxs) (rf :: rx :: rxs) |> GoalSet.of_list_eq)
    | _ -> None)

let rec gfp assumptions goals =
  GoalSet.is_empty goals
  ||
  let goal = GoalSet.choose goals in
  match support goal with
  | None -> false
  | Some sub_goals ->
    gfp
      (GoalSet.add goal assumptions)
      (GoalSet.union
         (GoalSet.remove goal goals)
         (GoalSet.diff sub_goals assumptions))

let sub_of_norm sub sup =
  (sub, sup) |> regularize_free_vars |> GoalSet.singleton |> gfp GoalSet.empty

let equal_of_norm lhs rhs =
  (lhs, rhs) |> regularize_free_vars |> GoalSet.singleton_eq
  |> gfp GoalSet.empty

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
  | `Product (at, ls) -> `Product (at, ls |> List.map (Pair.map id to_strict))
  | `Sum (at, ls) -> `Sum (at, ls |> List.map (Pair.map id to_strict))
  | `Lazy t -> Lazy.force t |> to_strict

let rec to_lazy t =
  match t with
  | `Mu (at, t) -> `Mu (at, to_lazy t)
  | `Const _ as inn -> inn
  | `Var _ as inn -> inn
  | `Lam (at, i, k, t) -> `Lam (at, i, k, to_lazy t)
  | `App (at, f, x) -> `App (at, to_lazy f, to_lazy x)
  | `ForAll (at, t) -> `ForAll (at, to_lazy t)
  | `Exists (at, t) -> `Exists (at, to_lazy t)
  | `Arrow (at, d, c) -> `Arrow (at, to_lazy d, to_lazy c)
  | `Product (at, ls) -> `Product (at, ls |> List.map (Pair.map id to_lazy))
  | `Sum (at, ls) -> `Sum (at, ls |> List.map (Pair.map id to_lazy))

module GoalMap = Map.Make (Goal)

let join_of_norm lhs rhs =
  let exception Fail in
  let at = Loc.dummy in
  let joins = ref GoalMap.empty in
  let meets = ref GoalMap.empty in
  let memo_in goals goal thunk =
    match GoalMap.find_opt goal !goals with
    | Some result -> result
    | None ->
      let result = `Lazy (lazy (thunk ())) in
      goals := GoalMap.add goal result !goals;
      result
  in
  let rec join ((lhs, rhs) as goal) =
    memo_in joins goal @@ fun () ->
    if sub_of_norm lhs rhs then
      to_lazy rhs
    else if sub_of_norm rhs lhs then
      to_lazy lhs
    else
      match goal with
      | `Arrow (_, lhs_d, lhs_c), `Arrow (_, rhs_d, rhs_c) ->
        `Arrow (at, meet (lhs_d, rhs_d), join (lhs_c, rhs_c))
      | `Product (_, lhs_ls), `Product (_, rhs_ls) ->
        `Product (at, intersection join [] (lhs_ls, rhs_ls))
      | `Sum (_, lhs_ls), `Sum (_, rhs_ls) ->
        `Sum (at, union join [] (lhs_ls, rhs_ls))
      | `Lam (_, lhs_i, lhs_k, lhs_t), `Lam (_, rhs_i, rhs_k, rhs_t) ->
        binding join lhs_i lhs_k lhs_t rhs_i rhs_k rhs_t
      | `ForAll (_, lhs_t), `ForAll (_, rhs_t) ->
        `ForAll (at, join (lhs_t, rhs_t))
      | `Exists (_, lhs_t), `Exists (_, rhs_t) ->
        `Exists (at, join (lhs_t, rhs_t))
      | _ -> mu join lhs rhs
  and meet ((lhs, rhs) as goal) =
    memo_in meets goal @@ fun () ->
    if sub_of_norm lhs rhs then
      to_lazy lhs
    else if sub_of_norm rhs lhs then
      to_lazy rhs
    else
      match goal with
      | `Arrow (_, lhs_d, lhs_c), `Arrow (_, rhs_d, rhs_c) ->
        `Arrow (at, join (lhs_d, rhs_d), meet (lhs_c, rhs_c))
      | `Product (_, lhs_ls), `Product (_, rhs_ls) ->
        `Product (at, union meet [] (lhs_ls, rhs_ls))
      | `Sum (_, lhs_ls), `Sum (_, rhs_ls) ->
        `Sum (at, intersection meet [] (lhs_ls, rhs_ls))
      | `Lam (_, lhs_i, lhs_k, lhs_t), `Lam (_, rhs_i, rhs_k, rhs_t) ->
        binding meet lhs_i lhs_k lhs_t rhs_i rhs_k rhs_t
      | `ForAll (_, lhs_t), `ForAll (_, rhs_t) ->
        `ForAll (at, meet (lhs_t, rhs_t))
      | `Exists (_, lhs_t), `Exists (_, rhs_t) ->
        `Exists (at, meet (lhs_t, rhs_t))
      | _ -> mu meet lhs rhs
  and intersection op ls = function
    | ( ((lhs_l, lhs_t) :: lhs_ls as lhs_lls),
        ((rhs_l, rhs_t) :: rhs_ls as rhs_lls) ) ->
      let c = Label.compare lhs_l rhs_l in
      if c < 0 then
        intersection op ls (lhs_ls, rhs_lls)
      else if 0 < c then
        intersection op ls (lhs_lls, rhs_ls)
      else
        intersection op ((lhs_l, op (lhs_t, rhs_t)) :: ls) (lhs_ls, rhs_ls)
    | [], _ | _, [] -> List.rev ls
  and union op ls = function
    | ( ((lhs_l, lhs_t) :: lhs_ls as lhs_lls),
        ((rhs_l, rhs_t) :: rhs_ls as rhs_lls) ) ->
      let c = Label.compare lhs_l rhs_l in
      if c < 0 then
        union op ((lhs_l, to_lazy lhs_t) :: ls) (lhs_ls, rhs_lls)
      else if 0 < c then
        union op ((rhs_l, to_lazy rhs_t) :: ls) (lhs_lls, rhs_ls)
      else
        union op ((lhs_l, op (lhs_t, rhs_t)) :: ls) (lhs_ls, rhs_ls)
    | (lhs_l, lhs_t) :: lhs_ls, [] ->
      union op ((lhs_l, to_lazy lhs_t) :: ls) (lhs_ls, [])
    | [], (rhs_l, rhs_t) :: rhs_ls ->
      union op ((rhs_l, to_lazy rhs_t) :: ls) ([], rhs_ls)
    | [], [] -> List.rev ls
  and binding op lhs_i lhs_k lhs_t rhs_i rhs_k rhs_t =
    if not (Kind.equal lhs_k rhs_k) then
      raise Fail;
    let i, goal = unify_vars lhs_i lhs_t rhs_i rhs_t in
    let assoc = free_vars_to_regular_assoc goal in
    op (Goal.map (to_subst assoc) goal)
    |> to_strict
    |> to_subst (assoc |> List.map Pair.swap)
    |> to_lazy
    |> fun t -> `Lam (at, i, lhs_k, t)
  and mu op lhs rhs =
    match (unapp lhs, unapp rhs) with
    | ((`Mu (lat, lf) as lmu), lxs), ((`Mu (rat, rf) as rmu), rxs) ->
      op (unfold lat lf lmu lxs, unfold rat rf rmu rxs)
    | ((`Mu (lat, lf) as lmu), lxs), _ -> op (unfold lat lf lmu lxs, rhs)
    | _, ((`Mu (rat, rf) as rmu), rxs) -> op (lhs, unfold rat rf rmu rxs)
    | _ -> raise Fail
  in
  try Some (join (lhs, rhs) |> to_strict) with Fail -> None
