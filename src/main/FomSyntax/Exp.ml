open FomSource

let bool = `Const (Loc.dummy, `Bool)
let int = `Const (Loc.dummy, `Int)

module Const = struct
  type t =
    [ `LitBool of bool
    | `LitNat of Bigint.t
    | `LitString of string
    | `OpArithAdd
    | `OpArithDiv
    | `OpArithMinus
    | `OpArithMul
    | `OpArithPlus
    | `OpArithRem
    | `OpArithSub
    | `OpCmpGt
    | `OpCmpGtEq
    | `OpCmpLt
    | `OpCmpLtEq
    | `OpEq of Typ.t
    | `OpEqNot of Typ.t
    | `OpLogicalAnd
    | `OpLogicalNot
    | `OpLogicalOr ]

  (* Typing *)

  let type_of at = function
    | `LitBool _ -> `Const (at, `Bool)
    | `LitNat _ -> `Const (at, `Int)
    | `LitString _ -> `Const (at, `String)
    | `OpArithAdd | `OpArithSub | `OpArithMul | `OpArithDiv | `OpArithRem ->
      Typ.arrow at int (Typ.arrow at int int)
    | `OpArithPlus | `OpArithMinus -> Typ.arrow at int int
    | `OpCmpLt | `OpCmpLtEq | `OpCmpGt | `OpCmpGtEq ->
      Typ.arrow at int (Typ.arrow at int bool)
    | `OpEq typ | `OpEqNot typ -> Typ.arrow at typ (Typ.arrow at typ bool)
    | `OpLogicalAnd | `OpLogicalOr -> Typ.arrow at bool (Typ.arrow at bool bool)
    | `OpLogicalNot -> Typ.arrow at bool bool

  let lit_false = `LitBool false
  let lit_true = `LitBool true
end

module Id = Id.Make ()

type t =
  [ `Const of Loc.t * Const.t
  | `Var of Loc.t * Id.t
  | `Lam of Loc.t * Id.t * Typ.t * t
  | `App of Loc.t * t * t
  | `Gen of Loc.t * Typ.Id.t * Kind.t * t
  | `Inst of Loc.t * t * Typ.t
  | `LetIn of Loc.t * Id.t * t * t
  | `Mu of Loc.t * t
  | `IfElse of Loc.t * t * t * t
  | `Product of Loc.t * (Label.t * t) list
  | `Select of Loc.t * t * Label.t
  | `Inject of Loc.t * Label.t * t * Typ.t
  | `Case of Loc.t * t * t
  | `Pack of Loc.t * Typ.t * t * Typ.t
  | `UnpackIn of Loc.t * Typ.Id.t * Id.t * t * t
  | `Target of Loc.t * Typ.t * string ]

let at = function
  | `Const (at, _)
  | `Var (at, _)
  | `Lam (at, _, _, _)
  | `App (at, _, _)
  | `Gen (at, _, _, _)
  | `Inst (at, _, _)
  | `LetIn (at, _, _, _)
  | `Mu (at, _)
  | `IfElse (at, _, _, _)
  | `Product (at, _)
  | `Select (at, _, _)
  | `Inject (at, _, _, _)
  | `Case (at, _, _)
  | `Pack (at, _, _, _)
  | `UnpackIn (at, _, _, _, _)
  | `Target (at, _, _) ->
    at

(* Macros *)

let var_of_label ({it; at} : Label.t) = `Var (at, Id.id at it)

let bin_op at lhs op rhs =
  match op with
  | `Const (_, `OpLogicalAnd) ->
    `IfElse (at, lhs, rhs, `Const (at, Const.lit_false))
  | `Const (_, `OpLogicalOr) ->
    `IfElse (at, lhs, `Const (at, Const.lit_true), rhs)
  | _ -> `App (at, `App (at, op, lhs), rhs)

let rec let_typ_in id typ exp =
  match exp with
  | `Const _ -> exp
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

let lit_bool at value =
  `Const (at, if value then Const.lit_true else Const.lit_false)

(* Substitution *)

let rec is_free i it =
  match it with
  | `Const _ | `Target _ -> false
  | `Var (_, i') -> Id.equal i' i
  | `Lam (_, i', _, e) -> (not (Id.equal i' i)) && is_free i e
  | `App (_, fn, arg) -> is_free i fn || is_free i arg
  | `Gen (_, _, _, e) -> is_free i e
  | `Inst (_, e, _) -> is_free i e
  | `LetIn (_, i', v, e) -> is_free i v || ((not (Id.equal i' i)) && is_free i e)
  | `Mu (_, e) -> is_free i e
  | `IfElse (_, c, t, e) -> is_free i c || is_free i t || is_free i e
  | `Product (_, fs) -> fs |> List.exists (fun (_, e) -> is_free i e)
  | `Select (_, e, _) -> is_free i e
  | `Inject (_, _, e, _) -> is_free i e
  | `Case (_, e, cs) -> is_free i e || is_free i cs
  | `Pack (_, _, e, _) -> is_free i e
  | `UnpackIn (_, _, i', v, e) ->
    is_free i v || ((not (Id.equal i' i)) && is_free i e)

let rec subst i the inn =
  match inn with
  | `Const _ | `Target _ -> inn
  | `Var (_, i') -> if Id.equal i' i then the else inn
  | `Lam (at, i', t, e) ->
    if Id.equal i' i then
      inn
    else if is_free i' the then
      let i'' = Id.freshen i' in
      let vi'' = `Var (at, i'') in
      `Lam (at, i'', t, subst i the (subst i' vi'' e))
    else
      `Lam (at, i', t, subst i the e)
  | `App (at, f, x) -> `App (at, subst i the f, subst i the x)
  | `Gen (at, i', k, e) -> `Gen (at, i', k, subst i the e)
  | `Inst (at, e, t) -> `Inst (at, subst i the e, t)
  | `LetIn (at, i', v, e) ->
    let v = subst i the v in
    if Id.equal i' i then
      `LetIn (at, i', v, e)
    else if is_free i' the then
      let i'' = Id.freshen i' in
      let vi'' = `Var (at, i'') in
      `LetIn (at, i'', v, subst i the (subst i' vi'' e))
    else
      `LetIn (at, i', v, subst i the e)
  | `Mu (at, e) -> `Mu (at, subst i the e)
  | `IfElse (at, c, t, e) ->
    `IfElse (at, subst i the c, subst i the t, subst i the e)
  | `Product (at, fs) ->
    `Product (at, fs |> List.map (fun (l, e) -> (l, subst i the e)))
  | `Select (at, e, l) -> `Select (at, subst i the e, l)
  | `Inject (at, l, e, t) -> `Inject (at, l, subst i the e, t)
  | `Case (at, e, cs) -> `Case (at, subst i the e, subst i the cs)
  | `Pack (at, t, e, et) -> `Pack (at, t, subst i the e, et)
  | `UnpackIn (at, ti', ei', v, e) ->
    let v = subst i the v in
    if Id.equal ei' i then
      `UnpackIn (at, ti', ei', v, e)
    else if is_free ei' the then
      let ei'' = Id.freshen ei' in
      let vei'' = `Var (at, ei'') in
      `UnpackIn (at, ti', ei'', v, subst i the (subst ei' vei'' e))
    else
      `UnpackIn (at, ti', ei', v, subst i the e)
