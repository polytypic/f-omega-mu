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
      find_opt_nested_arg_mu at' f (Kind.arity c_kind)
      |> Option.iter (Error.mu_nested at' typ);
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
    let* _ = check star d in
    let* _ = check star c in
    return star
  | `Product (at', ls) | `Sum (at', ls) ->
    let star = `Star at' in
    let* _ = ls |> traverse (snd >> check star) in
    return star

and check expected t =
  let open Reader in
  let* actual = infer t in
  if not (Kind.equal expected actual) then
    Error.kind_mismatch (at t) expected actual;
  return actual

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

let rec is_contractive ids typ =
  match unapp typ with
  | `Mu (_, `Lam (_, id, _, f)), xs -> (
    match app Loc.dummy f xs |> norm |> unapp with
    | `Var (_, id'), _ when Id.equal id' id || IdSet.mem id' ids -> false
    | typ, _ -> is_contractive (IdSet.add id ids) typ)
  | _ -> true

let is_contractive = is_contractive IdSet.empty

let rec unfold_of_norm typ =
  match unapp typ with
  | (`Mu (at', f) as mu), xs -> unfold_of_norm (unfold at' f mu xs)
  | _ -> typ

let unfold_of_norm typ = if is_contractive typ then unfold_of_norm typ else typ

module Goals = struct
  include Set.Make (Compare.Pair (FomAST.Typ) (FomAST.Typ))

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

let regularize_free_vars (lhs, rhs) =
  let subst =
    IdSet.union (free lhs) (free rhs)
    |> IdSet.elements
    |> List.mapi (fun i v ->
           (v, `Var (Id.at v, Id.of_string (Id.at v) ("#" ^ string_of_int i))))
    |> List.to_seq |> Env.of_seq |> subst_par
  in
  (subst lhs, subst rhs)

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
  loop Goals.empty ls ms

let support (lhs, rhs) =
  match (lhs, rhs) with
  | `Const (_, lhs_const), `Const (_, rhs_const)
    when Const.equal lhs_const rhs_const ->
    Some Goals.empty
  | `Arrow (_, lhs_d, lhs_c), `Arrow (_, rhs_d, rhs_c) ->
    Some ([(rhs_d, lhs_d); (lhs_c, rhs_c)] |> Goals.of_list)
  | `Product (_, lhs_ls), `Product (_, rhs_ls) ->
    intersect_labels Goals.add rhs_ls lhs_ls
  | `Sum (_, lhs_ls), `Sum (_, rhs_ls) ->
    intersect_labels Goals.add_inv lhs_ls rhs_ls
  | `Var (_, lhs_id), `Var (_, rhs_id) when Id.equal lhs_id rhs_id ->
    Some Goals.empty
  | `ForAll (_, lhs), `ForAll (_, rhs) | `Exists (_, lhs), `Exists (_, rhs) ->
    Some (Goals.singleton (lhs, rhs))
  | `Lam (_, lhs_id, lhs_kind, lhs_typ), `Lam (_, rhs_id, rhs_kind, rhs_typ)
    when Kind.equal lhs_kind rhs_kind ->
    let entry =
      if Id.equal lhs_id rhs_id then
        (lhs_typ, rhs_typ)
      else
        let new_var = `Var (Loc.dummy, Id.fresh Loc.dummy) in
        (subst lhs_id new_var lhs_typ, subst rhs_id new_var rhs_typ)
    in
    Some (entry |> regularize_free_vars |> Goals.singleton)
  | _ -> (
    match (unapp lhs, unapp rhs) with
    | (`Mu _, _), (`Mu _, _)
      when (not (is_contractive lhs)) && not (is_contractive rhs) ->
      Some Goals.empty
    | ((`Mu (lat, lf) as lmu), lxs), ((`Mu (rat, rf) as rmu), rxs)
      when is_contractive lhs && is_contractive rhs ->
      Some (Goals.singleton (unfold lat lf lmu lxs, unfold rat rf rmu rxs))
    | ((`Mu (lat, lf) as lmu), lxs), _ when is_contractive lhs ->
      Some (Goals.singleton (unfold lat lf lmu lxs, rhs))
    | _, ((`Mu (rat, rf) as rmu), rxs) when is_contractive rhs ->
      Some (Goals.singleton (lhs, unfold rat rf rmu rxs))
    | (lf, lx :: lxs), (rf, rx :: rxs) when List.length lxs = List.length rxs ->
      Some (List.combine (lf :: lx :: lxs) (rf :: rx :: rxs) |> Goals.of_list_eq)
    | _ -> None)

let rec gfp assumptions goals =
  Goals.is_empty goals
  ||
  let goal = Goals.choose goals in
  match support goal with
  | None -> false
  | Some sub_goals ->
    gfp
      (Goals.add goal assumptions)
      (Goals.union (Goals.remove goal goals) (Goals.diff sub_goals assumptions))

let sub_of_norm sub sup =
  (sub, sup) |> regularize_free_vars |> Goals.singleton |> gfp Goals.empty

let equal_of_norm lhs rhs =
  (lhs, rhs) |> regularize_free_vars |> Goals.singleton_eq |> gfp Goals.empty
