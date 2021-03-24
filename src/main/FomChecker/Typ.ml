open FomSource
open FomDiag
open FomAnnot
open FomBasis

(* *)

include FomAST.Typ

(* *)

module Env = Map.Make (Id)

let rec find_map_from_all_apps_of i' p = function
  | `Const _ -> None
  | `Lam (_, i, _, t) ->
    if Id.equal i i' then
      None
    else
      find_map_from_all_apps_of i' p t
  | `Mu (_, t) | `ForAll (_, t) | `Exists (_, t) ->
    find_map_from_all_apps_of i' p t
  | t -> (
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

let rec check typ : _ -> Kind.t =
  let open Reader in
  let quantifier symbol f =
    let* f_kind = check f in
    match f_kind with
    | `Arrow (_, _, (`Star _ as c_kind)) -> return c_kind
    | _ -> Error.quantifier_kind (at typ) symbol f f_kind
  in
  match typ with
  | `Mu (at', f) -> (
    let* f_kind = check f in
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
      let* () = Annot.Typ.use i def in
      return i_kind)
  | `Lam (at', d, d_kind, r) ->
    let* () = Annot.Typ.def d d_kind in
    let* r_kind e = Env.add d (d, d_kind) |> e#map_typ_env |> check r in
    return (`Arrow (at', d_kind, r_kind))
  | `App (at', f, x) -> (
    let* f_kind = check f in
    match f_kind with
    | `Star _ -> Error.app_of_kind_star at' f x
    | `Arrow (_, d_kind, c_kind) ->
      let* x_kind = check x in
      if not (Kind.equal d_kind x_kind) then
        Error.app_kind_mismatch at' f d_kind x x_kind;
      return c_kind)
  | `ForAll (_, f) -> quantifier FomPP.for_all f
  | `Exists (_, f) -> quantifier FomPP.exists f

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
  | `ForAll (at', _) -> return (`Star at')
  | `Exists (at', _) -> return (`Star at')

and kind_of_cod checked_typ : _ -> Kind.t =
  let open Reader in
  let* f_kind = kind_of checked_typ in
  match f_kind with
  | `Star _ -> failwith "Impossible"
  | `Arrow (_, _, c_kind) -> return c_kind

let unfold at f mu xs = FomAST.Typ.app at (`App (at, f, mu)) xs |> norm

let rec head_of_norm typ =
  match unapp typ with
  | (`Mu (at', f) as mu), xs -> head_of_norm (unfold at' f mu xs)
  | _ -> typ

module Ids = Set.Make (Id)

let rec is_contractive ids typ =
  match unapp typ with
  | `Mu (_, `Lam (_, id, _, f)), xs -> (
    match app Loc.dummy f xs |> norm |> unapp with
    | `Var (_, id'), _ when Id.equal id' id || Ids.mem id' ids -> false
    | typ, _ -> is_contractive (Ids.add id ids) typ)
  | _ -> true

let is_contractive = is_contractive Ids.empty

module Set = Set.Make (Compare.Pair (FomAST.Typ) (FomAST.Typ))

let support (lhs, rhs) =
  if 0 = FomAST.Typ.compare lhs rhs then
    Some Set.empty
  else
    match (lhs, rhs) with
    | `Const (_, lhs_const), `Const (_, rhs_const)
      when Const.equal lhs_const rhs_const ->
      Some Set.empty
    | `Var (_, lhs_id), `Var (_, rhs_id) when Id.equal lhs_id rhs_id ->
      Some Set.empty
    | `ForAll (_, lhs), `ForAll (_, rhs) | `Exists (_, lhs), `Exists (_, rhs) ->
      Some (Set.singleton (lhs, rhs))
    | `Lam (_, lhs_id, lhs_kind, lhs_typ), `Lam (_, rhs_id, rhs_kind, rhs_typ)
      when Kind.equal lhs_kind rhs_kind ->
      Some
        (Set.singleton
           (if Id.equal lhs_id rhs_id then
              (lhs_typ, rhs_typ)
           else
             let new_var = `Var (Loc.dummy, Id.fresh Loc.dummy) in
             (subst lhs_id new_var lhs_typ, subst rhs_id new_var rhs_typ)))
    | _ -> (
      find_opt_nested_arg lhs
      |> Option.iter (fun arg -> Error.mu_nested (at lhs) lhs arg);
      find_opt_nested_arg rhs
      |> Option.iter (fun arg -> Error.mu_nested (at rhs) rhs arg);
      match (unapp lhs, unapp rhs) with
      | (`Mu _, _), (`Mu _, _)
        when (not (is_contractive lhs)) && not (is_contractive rhs) ->
        Some Set.empty
      | ((`Mu (lat, lf) as lmu), lxs), ((`Mu (rat, rf) as rmu), rxs)
        when is_contractive lhs && is_contractive rhs ->
        Some (Set.singleton (unfold lat lf lmu lxs, unfold rat rf rmu rxs))
      | ((`Mu (lat, lf) as lmu), lxs), _ when is_contractive lhs ->
        Some (Set.singleton (unfold lat lf lmu lxs, rhs))
      | _, ((`Mu (rat, rf) as rmu), rxs) when is_contractive rhs ->
        Some (Set.singleton (lhs, unfold rat rf rmu rxs))
      | (lf, lxs), (rf, rxs) when List.length lxs = List.length rxs ->
        Some (List.combine (lf :: lxs) (rf :: rxs) |> Set.of_list)
      | _ -> None)

let rec gfp assumptions goals =
  Set.is_empty goals
  ||
  let goal = Set.choose goals in
  match support goal with
  | None -> false
  | Some sub_goals ->
    gfp (Set.add goal assumptions)
      (Set.union (Set.remove goal goals) (Set.diff sub_goals assumptions))

let equal_of_norm lhs rhs = gfp Set.empty (Set.singleton (lhs, rhs))
