open FomBasis
open FomAST
open FomAnnot

let rec type_of_pat_lam = function
  | `Id (_, _, t) -> t
  | `Product (at, fs) ->
    Typ.product at
      (fs
      |> List.map
           (Pair.map id @@ function `Pat p -> type_of_pat_lam p | `Ann t -> t))
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

let elaborate_typ t r =
  let replaced i t = Annot.Typ.use i (Typ.at t) r in
  Typ.subst_par ~replaced r#get_typ_aliases t

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
    fun r ->
      let replaced i t = Annot.Typ.use i (Typ.at t) r in
      `Const (at, Exp.Const.subst_par ~replaced r#get_typ_aliases c)
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
  | `LetTypIn (_, i, kO, t, e) ->
    let* () = Annot.Typ.alias i t in
    let* t = elaborate_typ t in
    let t =
      match kO with
      | None -> t
      | Some k ->
        let at = Kind.at k in
        let i = Typ.Id.fresh at in
        `App (at, `Lam (at, i, k, `Var (at, i)), t)
    in
    fun r ->
      t
      |> Typ.set_at (Typ.Id.at i)
      |> Typ.Env.add i |> r#map_typ_aliases |> elaborate e
  | `LetTypRecIn (_, bs, e) ->
    let* assoc =
      bs
      |> traverse (fun ((i, k), t) ->
             let at = Typ.Id.at i in
             let t = `Mu (at, `Lam (at, i, k, t)) in
             let* () = Annot.Typ.alias i t in
             let* t = elaborate_typ t in
             return (i, t))
    in
    let env = assoc |> List.to_seq |> Typ.Env.of_seq in
    let* replaced r i t = Annot.Typ.use i (Typ.at t) r in
    let env = env |> Typ.Env.map (Typ.subst_rec ~replaced env) in
    fun r ->
      Typ.Env.union (fun _ v _ -> Some v) env
      |> r#map_typ_aliases |> elaborate e
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
      |> traverse (fun (l, e) ->
             let* e = elaborate e in
             return (l, e))
    in
    return @@ `Product (at, fs)
  | `Select (at, e, l) ->
    let* e = elaborate e in
    return @@ `Select (at, e, l)
  | `Inject (at, l, e) ->
    let* e = elaborate e in
    return @@ `Inject (at, l, e)
  | `Case (at, s, cs) ->
    let* s = elaborate s in
    let* cs = elaborate cs in
    return @@ `Case (at, s, cs)
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
