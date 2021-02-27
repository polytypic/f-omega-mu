open FomAST

let rec let_typ_in id typ exp =
  match exp with
  | `Const (at, c) -> `Const (at, Exp.Const.subst id typ c)
  | `Var _ -> exp
  | `Lam (at, i, t, e) -> `Lam (at, i, Typ.subst id typ t, let_typ_in id typ e)
  | `App (at, fn, arg) -> `App (at, let_typ_in id typ fn, let_typ_in id typ arg)
  | `Gen (at, i, k, e) ->
    if Typ.Id.equal i id then
      exp
    else if Typ.is_free i typ then
      let i' = Typ.Id.freshen i in
      let vi' = `Var (at, i') in
      `Gen (at, i', k, let_typ_in id typ (let_typ_in i' vi' exp))
    else
      `Gen (at, i, k, let_typ_in id typ e)
  | `Inst (at, e, t) -> `Inst (at, let_typ_in id typ e, Typ.subst id typ t)
  | `LetIn (at, i, v, e) ->
    `LetIn (at, i, let_typ_in id typ v, let_typ_in id typ e)
  | `Mu (at, exp) -> `Mu (at, let_typ_in id typ exp)
  | `IfElse (at, c, t, e) ->
    `IfElse (at, let_typ_in id typ c, let_typ_in id typ t, let_typ_in id typ e)
  | `Product (at, fs) ->
    `Product (at, fs |> List.map (fun (l, e) -> (l, let_typ_in id typ e)))
  | `Select (at, e, l) -> `Select (at, let_typ_in id typ e, l)
  | `Inject (at, l, e, t) ->
    `Inject (at, l, let_typ_in id typ e, Typ.subst id typ t)
  | `Case (at, e, cs) -> `Case (at, let_typ_in id typ e, let_typ_in id typ cs)
  | `Pack (at, t, e, et) ->
    `Pack (at, Typ.subst id typ t, let_typ_in id typ e, Typ.subst id typ et)
  | `UnpackIn (at, ti', ei', v, e) ->
    let v = let_typ_in id typ v in
    if Typ.Id.equal ti' id then
      `UnpackIn (at, ti', ei', v, e)
    else if Typ.is_free ti' typ then
      let ti'' = Typ.Id.freshen ti' in
      let vti'' = `Var (at, ti'') in
      `UnpackIn (at, ti'', ei', v, let_typ_in id typ (let_typ_in ti' vti'' e))
    else
      `UnpackIn (at, ti', ei', v, let_typ_in id typ e)
  | `Target (at, t, s) -> `Target (at, Typ.subst id typ t, s)

let rec elaborate = function
  | `Const _ as ast -> ast
  | `Var _ as ast -> ast
  | `Lam (at, i, t, e) ->
    let e = elaborate e in
    `Lam (at, i, t, e)
  | `App (at, f, x) ->
    let f = elaborate f in
    let x = elaborate x in
    `App (at, f, x)
  | `Gen (at, i, k, e) ->
    let e = elaborate e in
    `Gen (at, i, k, e)
  | `Inst (at, e, t) ->
    let e = elaborate e in
    `Inst (at, e, t)
  | `LetIn (at, i, v, e) ->
    let v = elaborate v in
    let e = elaborate e in
    `LetIn (at, i, v, e)
  | `LetTypIn (_, i, t, e) ->
    let e = elaborate e in
    let_typ_in i t e
  | `Mu (at, e) ->
    let e = elaborate e in
    `Mu (at, e)
  | `IfElse (at, c, t, e) ->
    let c = elaborate c in
    let t = elaborate t in
    let e = elaborate e in
    `IfElse (at, c, t, e)
  | `Product (at, fs) ->
    `Product
      ( at,
        fs
        |> List.map (fun (l, e) ->
               let e = elaborate e in
               (l, e)) )
  | `Select (at, e, l) ->
    let e = elaborate e in
    `Select (at, e, l)
  | `Inject (at, l, e, t) ->
    let e = elaborate e in
    `Inject (at, l, e, t)
  | `Case (at, s, cs) ->
    let s = elaborate s in
    let cs = elaborate cs in
    `Case (at, s, cs)
  | `Pack (at, t, e, x) ->
    let e = elaborate e in
    `Pack (at, t, e, x)
  | `UnpackIn (at, ti, ei, v, e) ->
    let v = elaborate v in
    let e = elaborate e in
    `UnpackIn (at, ti, ei, v, e)
  | `Target _ as ast -> ast
