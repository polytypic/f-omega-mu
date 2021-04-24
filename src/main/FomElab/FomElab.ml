open FomBasis
open FomAST
open FomAnnot

let rec type_of_pat_lam = function
  | `Id (_, _, t) -> t
  | `Product (at, fs) ->
    Typ.product at
      (fs
      |> List.map
           (Pair.map Fun.id @@ function
            | `Pat p -> type_of_pat_lam p
            | `Ann t -> t))
  | `Pack (_, _, _, t) -> t

let rec elaborate_pat p' e' = function
  | `Id (at, i, _) -> `LetIn (at, i, p', e')
  | `Product (at, fs) ->
    fs |> List.rev
    |> List.fold_left
         (fun e' -> function
           | l, `Pat p ->
             let i =
               Exp.Id.freshen (Exp.Id.of_name (Label.at l) (Label.name l))
             in
             `LetIn
               (at, i, `Select (at, p', l), elaborate_pat (`Var (at, i)) e' p)
           | l, `Ann _ ->
             `LetIn
               ( at,
                 Exp.Id.of_name (Label.at l) (Label.name l),
                 `Select (at, p', l),
                 e' ))
         e'
  | `Pack (at, `Id (_, i, _), t, _) -> `UnpackIn (at, t, i, p', e')
  | `Pack (at, p, t, _) ->
    let i = Exp.Id.fresh (FomCST.Exp.Pat.at p) in
    `UnpackIn (at, t, i, p', elaborate_pat (`Var (at, i)) e' p)

let rec elaborate_def =
  let open Reader in
  function
  | `Typ (_, i, kO, t) ->
    let* t = elaborate_typ t in
    env_as (Annot.Typ.alias i t)
    >>
    let t =
      match kO with
      | None -> t
      | Some k ->
        let at = Kind.at k in
        let i = Typ.Id.fresh at in
        `App (at, `Lam (at, i, k, `Var (at, i)), t)
    in
    env_as @@ fun r ->
    Typ.Env.add i (Typ.set_at (Typ.Id.at i) t) r#get_typ_aliases
  | `TypRec (_, bs) ->
    let* assoc =
      bs
      |> traverse @@ fun ((i, k), t) ->
         let at = Typ.Id.at i in
         let t = `Mu (at, `Lam (at, i, k, t)) in
         let* t = elaborate_typ t in
         env_as (Annot.Typ.alias i t) >> return (i, t)
    in
    let env = assoc |> List.to_seq |> Typ.Env.of_seq in
    let* r = env_as Fun.id in
    let replaced i t = Annot.Typ.use i (Typ.at t) r in
    let env = env |> Typ.Env.map (Typ.subst_rec ~replaced env) in
    env_as @@ fun r -> Typ.Env.union (fun _ v _ -> Some v) env r#get_typ_aliases
  | `Include (at', p) -> (
    let filename = FomModules.resolve at' p ~ext:FomModules.inc_ext in
    let* env_opt =
      env_as @@ fun r -> FomCST.Typ.IncludeMap.find_opt filename r#get_includes
    in
    match env_opt with
    | None -> failwithf "include %s not found" filename
    | Some env ->
      env_as @@ fun r ->
      FomAST.Typ.Env.merge
        (fun _ l r ->
          match (l, r) with
          | Some l, _ -> Some l
          | _, Some r -> Some r
          | _, _ -> None)
        env r#get_typ_aliases)

and elaborate_typ =
  let open Reader in
  function
  | `Mu (at', t) ->
    let* t = elaborate_typ t in
    return @@ `Mu (at', t)
  | `Const (at', c) -> return @@ `Const (at', c)
  | `Var (at', i) -> (
    let* t_opt = env_as @@ fun r -> Typ.Env.find_opt i r#get_typ_aliases in
    match t_opt with
    | None -> return @@ `Var (at', i)
    | Some t -> env_as (Annot.Typ.use i (Typ.at t)) >> return t)
  | `Lam (at', i, k, t) ->
    with_env (fun r -> Typ.Env.remove i |> r#map_typ_aliases)
    @@ let* exists =
         env_as @@ fun r ->
         Typ.Env.exists (fun _ t' -> Typ.is_free i t') r#get_typ_aliases
       in
       if exists then
         let i' = Typ.Id.freshen i in
         let v' = `Var (at', i') in
         let* t =
           elaborate_typ t
           |> with_env @@ fun r -> Typ.Env.add i' v' |> r#map_typ_aliases
         in
         return @@ `Lam (at', i', k, t)
       else
         let* t = elaborate_typ t in
         return @@ `Lam (at', i, k, t)
  | `App (at', f, x) ->
    let* f = elaborate_typ f in
    let* x = elaborate_typ x in
    return @@ `App (at', f, x)
  | `ForAll (at', t) ->
    let* t = elaborate_typ t in
    return @@ `ForAll (at', t)
  | `Exists (at', t) ->
    let* t = elaborate_typ t in
    return @@ `Exists (at', t)
  | `Arrow (at', d, c) ->
    let* d = elaborate_typ d in
    let* c = elaborate_typ c in
    return @@ `Arrow (at', d, c)
  | `Product (at', ls) ->
    let* ls =
      ls
      |> traverse @@ fun (l, t) ->
         let* t = elaborate_typ t in
         return (l, t)
    in
    return @@ `Product (at', ls)
  | `Sum (at', ls) ->
    let* ls =
      ls
      |> traverse @@ fun (l, t) ->
         let* t = elaborate_typ t in
         return (l, t)
    in
    return @@ `Sum (at', ls)
  | `LetDefIn (_, def, e) ->
    let* typ_aliases = elaborate_def def in
    elaborate_typ e
    |> with_env @@ fun r -> r#map_typ_aliases @@ Fun.const typ_aliases

let rec elaborate_defs =
  let open Reader in
  function
  | [] -> env_as @@ fun r -> r#get_typ_aliases
  | def :: defs ->
    let* typ_aliases = elaborate_def def in
    elaborate_defs defs
    |> with_env @@ fun r -> r#map_typ_aliases @@ Fun.const typ_aliases

let maybe_annot e tO =
  let open Reader in
  match tO with
  | None -> return e
  | Some t ->
    let* t = elaborate_typ t in
    let at = Typ.at t in
    let i = Exp.Id.fresh at in
    return @@ `App (at, `Lam (at, i, t, `Var (at, i)), e)

let rec elaborate =
  let open Reader in
  function
  | `Const (at, c) ->
    let* c = c |> Exp.Const.traverse_typ elaborate_typ in
    return @@ `Const (at, c)
  | `Var _ as ast -> return ast
  | `Target (at, t, s) ->
    let* t = elaborate_typ t in
    return @@ `Target (at, t, s)
  | `Lam (at, i, t, e) | `LamPat (at, `Id (_, i, t), e) ->
    let* t = elaborate_typ t in
    let* e = elaborate e in
    return @@ `Lam (at, i, t, e)
  | `App (at, f, x) ->
    let* f = elaborate f in
    let* x = elaborate x in
    return @@ `App (at, f, x)
  | `Gen (at, i, k, e) ->
    let* e = elaborate e in
    return @@ `Gen (at, i, k, e)
  | `Inst (at, e, t) ->
    let* e = elaborate e in
    let* t = elaborate_typ t in
    return @@ `Inst (at, e, t)
  | `LetIn (at, i, v, e) ->
    let* v = elaborate v in
    let* e = elaborate e in
    return @@ `LetIn (at, i, v, e)
  | `LetPat (at, `Id (_, i, _), tO, v, e) ->
    let* v = elaborate v in
    let* v = maybe_annot v tO in
    let* e = elaborate e in
    return @@ `LetIn (at, i, v, e)
  | `LetDefIn (_, def, e) ->
    let* typ_aliases = elaborate_def def in
    elaborate e
    |> with_env @@ fun r -> r#map_typ_aliases @@ Fun.const typ_aliases
  | `Mu (at, e) ->
    let* e = elaborate e in
    return @@ `Mu (at, e)
  | `IfElse (at, c, t, e) ->
    let* c = elaborate c in
    let* t = elaborate t in
    let* e = elaborate e in
    return @@ `IfElse (at, c, t, e)
  | `Product (at, fs) ->
    let* fs =
      fs
      |> traverse @@ fun (l, e) ->
         let* e = elaborate e in
         return (l, e)
    in
    return @@ `Product (at, fs)
  | `Select (at, e, l) ->
    let* e = elaborate e in
    return @@ `Select (at, e, l)
  | `Inject (at, l, e) ->
    let* e = elaborate e in
    return @@ `Inject (at, l, e)
  | `Case (at, cs) ->
    let* cs = elaborate cs in
    return @@ `Case (at, cs)
  | `Pack (at, t, e, x) ->
    let* t = elaborate_typ t in
    let* e = elaborate e in
    let* x = elaborate_typ x in
    return @@ `Pack (at, t, e, x)
  | `UnpackIn (at, ti, ei, v, e) ->
    let* v = elaborate v in
    let* e = elaborate e in
    return @@ `UnpackIn (at, ti, ei, v, e)
  | `LetPat (at, `Pack (_, `Id (_, ei, _), ti, _), tO, v, e) ->
    let* v = elaborate v in
    let* v = maybe_annot v tO in
    let* e = elaborate e in
    return @@ `UnpackIn (at, ti, ei, v, e)
  | `LetPatRec (at, pvs, e) ->
    let p = pvs |> List.map fst |> FomCST.Exp.Pat.tuple at in
    let v = pvs |> List.map snd |> FomCST.Exp.tuple at in
    elaborate @@ `LetPat (at, p, None, `Mu (at, `LamPat (at, p, v)), e)
  | `LamPat (at, p, e) ->
    let* e = elaborate e in
    let t = type_of_pat_lam p in
    let* t = elaborate_typ t in
    let i = Exp.Id.fresh (FomCST.Exp.Pat.at p) in
    return @@ `Lam (at, i, t, elaborate_pat (`Var (at, i)) e p)
  | `LetPat (at, p, tO, v, e) ->
    let* v = elaborate v in
    let* v = maybe_annot v tO in
    let* e = elaborate e in
    let i = Exp.Id.fresh (FomCST.Exp.Pat.at p) in
    return @@ `LetIn (at, i, v, elaborate_pat (`Var (at, i)) e p)
  | `Annot (at, e, t) ->
    let* e = elaborate e in
    let* t = elaborate_typ t in
    let x = Exp.Id.fresh at in
    return @@ `App (at, `Lam (at, x, t, `Var (at, x)), e)
  | `AppL (at, x, f) | `AppR (at, f, x) ->
    let* x = elaborate x in
    let* f = elaborate f in
    return @@ `App (at, f, x)
  | `Import (at', p) -> (
    let filename = FomModules.resolve at' p ~ext:FomModules.mod_ext in
    let* i_opt =
      env_as @@ fun r -> FomCST.Exp.ImportMap.find_opt filename r#get_imports
    in
    match i_opt with
    | None -> failwithf "import %s not found" filename
    | Some i -> return @@ `Var (at', i))
