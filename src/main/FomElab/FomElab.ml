open FomBasis
open FomAST
open FomAnnot

let subst i' t' t r =
  Typ.subst ~replaced:(fun i -> Annot.Typ.use i i' r) i' t' t

let rec let_typ_in i' t' =
  let open Reader in
  function
  | `Const (at, c) -> return @@ `Const (at, Exp.Const.subst i' t' c)
  | `Var _ as exp -> return exp
  | `Lam (at, i, t, e) ->
    let* e = let_typ_in i' t' e in
    let* t = subst i' t' t in
    return @@ `Lam (at, i, t, e)
  | `App (at, fn, arg) ->
    let* fn = let_typ_in i' t' fn in
    let* arg = let_typ_in i' t' arg in
    return @@ `App (at, fn, arg)
  | `Gen (at, i, k, e) as exp ->
    if Typ.Id.equal i i' then
      return exp
    else if Typ.is_free i t' then
      let i' = Typ.Id.freshen i in
      let vi' = `Var (at, i') in
      let* e = let_typ_in i' vi' e in
      let* e = let_typ_in i' t' e in
      return @@ `Gen (at, i', k, e)
    else
      let* e = let_typ_in i' t' e in
      return @@ `Gen (at, i, k, e)
  | `Inst (at, e, t) ->
    let* e = let_typ_in i' t' e in
    let* t = subst i' t' t in
    return @@ `Inst (at, e, t)
  | `LetIn (at, i, v, e) ->
    let* v = let_typ_in i' t' v in
    let* e = let_typ_in i' t' e in
    return @@ `LetIn (at, i, v, e)
  | `Mu (at, e) ->
    let* e = let_typ_in i' t' e in
    return @@ `Mu (at, e)
  | `IfElse (at, c, t, e) ->
    let* c = let_typ_in i' t' c in
    let* t = let_typ_in i' t' t in
    let* e = let_typ_in i' t' e in
    return @@ `IfElse (at, c, t, e)
  | `Product (at, fs) ->
    let* fs =
      fs
      |> traverse (fun (l, e) ->
             let* e = let_typ_in i' t' e in
             return (l, e))
    in
    return @@ `Product (at, fs)
  | `Select (at, e, l) ->
    let* e = let_typ_in i' t' e in
    return @@ `Select (at, e, l)
  | `Inject (at, l, e, t) ->
    let* e = let_typ_in i' t' e in
    let* t = subst i' t' t in
    return @@ `Inject (at, l, e, t)
  | `Case (at, s, cs) ->
    let* s = let_typ_in i' t' s in
    let* cs = let_typ_in i' t' cs in
    return @@ `Case (at, s, cs)
  | `Pack (at, t, e, et) ->
    let* e = let_typ_in i' t' e in
    let* t = subst i' t' t in
    let* et = subst i' t' et in
    return @@ `Pack (at, t, e, et)
  | `UnpackIn (at, ti, ei, v, e) ->
    let* v = let_typ_in i' t' v in
    if Typ.Id.equal ti i' then
      return @@ `UnpackIn (at, ti, ei, v, e)
    else if Typ.is_free ti t' then
      let ti'' = Typ.Id.freshen ti in
      let vti'' = `Var (at, ti'') in
      let* e = let_typ_in ti vti'' e in
      let* e = let_typ_in i' t' e in
      return @@ `UnpackIn (at, ti'', ei, v, e)
    else
      let* e = let_typ_in i' t' e in
      return @@ `UnpackIn (at, ti, ei, v, e)
  | `Target (at, t, s) ->
    let* t = subst i' t' t in
    return @@ `Target (at, t, s)

let rec elaborate =
  let open Reader in
  function
  | `Const _ as ast -> return ast
  | `Var _ as ast -> return ast
  | `Target _ as ast -> return ast
  | `Lam (at, i, t, e) ->
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
    return @@ `Inst (at, e, t)
  | `LetIn (at, i, v, e) ->
    let* v = elaborate v in
    let* e = elaborate e in
    return @@ `LetIn (at, i, v, e)
  | `LetTypIn (_, (i : Typ.Id.t), t, e) ->
    let* () = Annot.Typ.alias i t in
    let* e = elaborate e in
    let_typ_in i t e
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
    return @@ `Inject (at, l, e, t)
  | `Case (at, s, cs) ->
    let* s = elaborate s in
    let* cs = elaborate cs in
    return @@ `Case (at, s, cs)
  | `Pack (at, t, e, x) ->
    let* e = elaborate e in
    return @@ `Pack (at, t, e, x)
  | `UnpackIn (at, ti, ei, v, e) ->
    let* v = elaborate v in
    let* e = elaborate e in
    return @@ `UnpackIn (at, ti, ei, v, e)
