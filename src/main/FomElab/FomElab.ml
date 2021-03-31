open FomBasis
open FomAST
open FomAnnot

let rec type_of_pat_lam = function
  | `Id (_, _, t) -> t
  | `Product (at, fs) ->
    Typ.product at
      (fs
      |> List.map (function
           | l, `Pat p -> (l, type_of_pat_lam p)
           | l, `Ann t -> (l, t)))
  | `Pack (_, _, _, t) -> t

let rec elaborate_pat p' e' = function
  | `Id (at, i, _) -> `LetIn (at, i, p', e')
  | `Product (at, fs) ->
    fs |> List.rev
    |> List.fold_left
         (fun e' -> function
           | l, `Pat p ->
             let i = Exp.Id.freshen (Exp.Id.id l.Label.at l.it) in
             `LetIn
               (at, i, `Select (at, p', l), elaborate_pat (`Var (at, i)) e' p)
           | l, `Ann _ ->
             `LetIn (at, Exp.Id.id l.Label.at l.it, `Select (at, p', l), e'))
         e'
  | `Pack (at, `Id (_, i, _), t, _) -> `UnpackIn (at, t, i, p', e')
  | `Pack (at, p, t, _) ->
    let i = Exp.Id.fresh (FomCST.Exp.Pat.at p) in
    `UnpackIn (at, t, i, p', elaborate_pat (`Var (at, i)) e' p)

let elaborate_typ t r =
  let replaced i t = Annot.Typ.use i (Typ.at t) r in
  Typ.subst_par ~replaced r#get_typ_aliases t

let rec elaborate =
  let open Reader in
  function
  | `Const (at, c) ->
    fun r ->
      let replaced i t = Annot.Typ.use i (Typ.at t) r in
      `Const (at, Exp.Const.subst_par ~replaced r#get_typ_aliases c)
  | `Var _ as ast -> return ast
  | `Target _ as ast -> return ast
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
  | `LetIn (at, i, v, e) | `LetPat (at, `Id (_, i, _), v, e) ->
    let* v = elaborate v in
    let* e = elaborate e in
    return @@ `LetIn (at, i, v, e)
  | `LetTypIn (_, i, t, e) ->
    let* () = Annot.Typ.alias i t in
    let* t = elaborate_typ t in
    fun r ->
      Typ.set_at i.at t |> Typ.Env.add i |> r#map_typ_aliases |> elaborate e
  | `LetTypRecIn (_, bs, e) ->
    let* assoc =
      bs
      |> traverse (fun ((i, k), t) ->
             let at = i.Typ.Id.at in
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
  | `Inject (at, l, e, t) ->
    let* e = elaborate e in
    let* t = elaborate_typ t in
    return @@ `Inject (at, l, e, t)
  | `Case (at, s, cs) ->
    let* s = elaborate s in
    let* cs = elaborate cs in
    return @@ `Case (at, s, cs)
  | `Pack (at, t, e, x) ->
    let* t = elaborate_typ t in
    let* e = elaborate e in
    let* x = elaborate_typ x in
    return @@ `Pack (at, t, e, x)
  | `UnpackIn (at, ti, ei, v, e)
  | `LetPat (at, `Pack (_, `Id (_, ei, _), ti, _), v, e) ->
    let* v = elaborate v in
    let* e = elaborate e in
    return @@ `UnpackIn (at, ti, ei, v, e)
  | `LetPatRec (at, pvs, e) ->
    let p = pvs |> List.map fst |> FomCST.Exp.Pat.tuple at in
    let v = pvs |> List.map snd |> FomCST.Exp.tuple at in
    elaborate @@ `LetPat (at, p, `Mu (at, `LamPat (at, p, v)), e)
  | `LamPat (at, p, e) ->
    let* e = elaborate e in
    let t = type_of_pat_lam p in
    let* t = elaborate_typ t in
    let i = Exp.Id.fresh (FomCST.Exp.Pat.at p) in
    return @@ `Lam (at, i, t, elaborate_pat (`Var (at, i)) e p)
  | `LetPat (at, p, v, e) ->
    let* v = elaborate v in
    let* e = elaborate e in
    let i = Exp.Id.fresh (FomCST.Exp.Pat.at p) in
    return @@ `LetIn (at, i, v, elaborate_pat (`Var (at, i)) e p)
