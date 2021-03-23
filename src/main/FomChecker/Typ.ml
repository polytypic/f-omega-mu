open FomSource
open FomDiag
open FomAnnot
open FomBasis

(* *)

include FomAST.Typ

(* *)

module Env = Map.Make (Id)

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
    match FomAST.Typ.app Loc.dummy f xs |> FomAST.Typ.norm |> unapp with
    | `Var (_, id'), _ when Id.equal id' id || Ids.mem id' ids -> false
    | typ, _ -> is_contractive (Ids.add id ids) typ)
  | _ -> true

let is_contractive = is_contractive Ids.empty

module Set = Set.Make (Compare.Pair (FomAST.Typ) (FomAST.Typ))

let support (lhs, rhs) =
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
           let new_var = `Var (at lhs, Id.freshen lhs_id) in
           (subst lhs_id new_var lhs_typ, subst rhs_id new_var rhs_typ)))
  | `Mu (_, _), `Mu (_, _)
    when (not (is_contractive lhs)) && not (is_contractive rhs) ->
    Some Set.empty
  | _ -> (
    match (unapp lhs, unapp rhs) with
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

let rec gfp a xs =
  Set.is_empty xs
  ||
  let x = Set.choose xs in
  if Set.mem x a then
    gfp a (Set.remove x xs)
  else
    match support x with
    | None -> false
    | Some s -> gfp (Set.add x a) (Set.union xs s)

let equal_of_norm lhs rhs = gfp Set.empty (Set.singleton (lhs, rhs))
