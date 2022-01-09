open FomBasis
open FomPP
open FomSource
open FomAST

(* *)

module Var = Exp.Var
module VarSet = Set.Make (Var)
module VarMap = Map.Make (Var)

module Const = struct
  include Exp.Const

  type nonrec t = (int32, FomAST.Typ.Core.t) t

  let compare l r = compare' Int32.compare (Typ.compare :> Typ.Core.t cmp) l r

  let is_uop = function
    | `OpArithPlus | `OpArithMinus | `OpLogicalNot | `Keep _ -> true
    | `LitBool _ | `LitNat _ | `LitString _ | `OpArithAdd | `OpArithDiv
    | `OpArithMul | `OpArithRem | `OpArithSub | `OpCmpGt | `OpCmpGtEq | `OpCmpLt
    | `OpCmpLtEq | `OpEq _ | `OpEqNot _ | `OpLogicalAnd | `OpLogicalOr
    | `OpStringCat | `Target _ ->
      false

  let is_bop = function
    | `LitBool _ | `LitNat _ | `LitString _ | `Target _ | `OpArithMinus
    | `OpArithPlus | `OpLogicalNot | `Keep _ ->
      false
    | `OpArithAdd | `OpArithDiv | `OpArithMul | `OpArithRem | `OpArithSub
    | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpEq _ | `OpEqNot _
    | `OpLogicalAnd | `OpLogicalOr | `OpStringCat ->
      true

  let is_commutative = function
    | `OpArithAdd | `OpArithMul | `OpEq _ | `OpEqNot _ -> true
    | `LitBool _ | `LitNat _ | `LitString _ | `OpArithDiv | `OpArithMinus
    | `OpArithPlus | `OpArithRem | `OpArithSub | `OpCmpGt | `OpCmpGtEq
    | `OpCmpLt | `OpCmpLtEq | `OpLogicalAnd | `OpLogicalNot | `OpLogicalOr
    | `OpStringCat | `Keep _ | `Target _ ->
      false
end

type t =
  [ `App of t * t
  | `Case of t
  | `Const of Const.t
  | `IfElse of t * t * t
  | `Inject of Label.t * t
  | `Lam of Exp.Var.t * t
  | `Mu of t
  | `Product of (Label.t * t) list
  | `Select of t * t
  | `Var of Exp.Var.t ]

(* *)

let lam fn =
  let i = Var.fresh Loc.dummy in
  `Lam (i, fn @@ `Var i)

let unapp t =
  let rec loop xs = function `App (f, x) -> loop (x :: xs) f | f -> (f, xs) in
  loop [] t

let apps f = List.fold_left (fun f x -> `App (f, x)) f

let unlam t =
  let rec loop is = function
    | `Lam (i, e) -> loop (i :: is) e
    | e -> (List.rev is, e)
  in
  loop [] t

let and_uncase (is, e) =
  match e with
  | `Case _ as e ->
    let i = Var.fresh Loc.dummy in
    (is @ [i], `App (e, `Var i))
  | _ -> (is, e)

let lams is = List.fold_right (fun i e -> `Lam (i, e)) is

(* *)

let rec is_free i' = function
  | `Const _ -> false
  | `Var i -> Var.equal i' i
  | `Lam (i, e) -> (not (Var.equal i' i)) && is_free i' e
  | `App (f, x) -> is_free i' f || is_free i' x
  | `IfElse (c, t, e) -> is_free i' c || is_free i' t || is_free i' e
  | `Product fs -> fs |> List.exists (snd >>> is_free i')
  | `Mu e | `Inject (_, e) | `Case e -> is_free i' e
  | `Select (e, l) -> is_free i' e || is_free i' l

let eq l r =
  match (l, r) with
  | `App l, `App r -> eq'2 l r
  | `Case l, `Case r -> l == r
  | `Const l, `Const r -> l == r
  | `IfElse l, `IfElse r -> eq'3 l r
  | `Inject l, `Inject r -> eq'2 l r
  | `Lam l, `Lam r -> eq'2 l r
  | `Mu l, `Mu r -> l == r
  | `Product l, `Product r -> l == r
  | `Select l, `Select r -> eq'2 l r
  | `Var l, `Var r -> l == r
  | _ -> false

let keep_phys_eq' e e' = if e == e' || eq e e' then e else e'
let keep_phys_eq fn e = keep_phys_eq' e (fn e)

let rec subst_par env =
  keep_phys_eq @@ function
  | `Const _ as inn -> inn
  | `Var i as inn -> (
    match VarMap.find_opt i env with None -> inn | Some e -> e)
  | `Lam (i, e) as inn ->
    let env = VarMap.remove i env in
    if VarMap.is_empty env then inn
    else if VarMap.exists (fun i' t' -> is_free i t' && is_free i' e) env then
      let i' = Var.freshen i in
      let v' = `Var i' in
      `Lam (i', subst_par (VarMap.add i v' env) e)
    else `Lam (i, subst_par env e)
  | `App (f, x) -> `App (subst_par env f, subst_par env x)
  | `Mu e -> `Mu (subst_par env e)
  | `IfElse (c, t, e) ->
    `IfElse (subst_par env c, subst_par env t, subst_par env e)
  | `Product fs -> `Product (Row.map_phys_eq (subst_par env) fs)
  | `Select (e, l) -> `Select (subst_par env e, subst_par env l)
  | `Inject (l, e) -> `Inject (l, subst_par env e)
  | `Case cs -> `Case (subst_par env cs)

let subst i = function
  | `Var j when Var.equal i j -> id
  | the -> subst_par (VarMap.singleton i the)

(* *)

let rec bottomUp fn = function
  | (`Const _ | `Var _) as e -> fn e
  | `App (f, x) -> fn (`App (bottomUp fn f, bottomUp fn x))
  | `IfElse (c, t, e) ->
    fn (`IfElse (bottomUp fn c, bottomUp fn t, bottomUp fn e))
  | `Product fs -> fn (`Product (fs |> Row.map (bottomUp fn)))
  | `Mu e -> fn (`Mu (bottomUp fn e))
  | `Lam (i, e) -> fn (`Lam (i, bottomUp fn e))
  | `Inject (l, e) -> fn (`Inject (l, bottomUp fn e))
  | `Select (e, l) -> fn (`Select (bottomUp fn e, bottomUp fn l))
  | `Case cs -> fn (`Case (bottomUp fn cs))

let size t =
  t
  |> bottomUp @@ function
     | `Const _ | `Var _ -> 1
     | `App (f, x) -> f + x + 1
     | `IfElse (c, t, e) -> c + t + e + 1
     | `Product fs -> fs |> List.fold_left (fun s (_, e) -> s + e) 1
     | `Mu e | `Lam (_, e) | `Inject (_, e) -> e + 1
     | `Select (e, l) -> e + l + 1
     | `Case cs -> cs + 1

(* *)

let index = function
  | `App _ -> 0
  | `Case _ -> 1
  | `Const _ -> 2
  | `IfElse _ -> 3
  | `Inject _ -> 4
  | `Lam _ -> 5
  | `Mu _ -> 6
  | `Product _ -> 7
  | `Select _ -> 8
  | `Var _ -> 9

let rec compare (l : t) (r : t) =
  if l == r then 0
  else
    match (l, r) with
    | `App (fl, xl), `App (fr, xr) -> compare xl xr <>? fun () -> compare fl fr
    | `Case l, `Case r | `Mu l, `Mu r -> compare l r
    | `Const l, `Const r -> Const.compare l r
    | `IfElse (cl, tl, el), `IfElse (cr, tr, er) ->
      compare cl cr <>? fun () ->
      compare tl tr <>? fun () -> compare el er
    | `Select (el, ll), `Select (er, lr) ->
      compare ll lr <>? fun () -> compare el er
    | `Inject (ll, el), `Inject (lr, er) ->
      Label.compare ll lr <>? fun () -> compare el er
    | `Lam (vl, el), `Lam (vr, er) ->
      if Var.equal vl vr then compare el er
      else
        let v = `Var (Var.fresh Loc.dummy) in
        compare (subst vl v el) (subst vr v er)
    | `Product lls, `Product rls ->
      List.compare_with
        (fun (ll, el) (lr, er) ->
          Label.compare ll lr <>? fun () -> compare el er)
        lls rls
    | `Var l, `Var r -> Var.compare l r
    | _ -> index l - index r

(* *)

let[@warning "-32"] rec pp : t -> document = function
  | `App (f, x) -> [pp f; space; pp x] |> concat |> egyptian parens 2
  | `Case t -> [utf8string "case"; space; pp t] |> concat
  | `Const c -> Const.pp' (Int32.to_string >>> utf8string) Typ.pp c
  | `IfElse (c, t, e) ->
    [
      [utf8string "if"; space; pp c; space] |> concat;
      [utf8string "then"; space; pp t; space] |> concat;
      [utf8string "else"; space; pp e; space] |> concat;
    ]
    |> concat |> egyptian parens 2
  | `Inject (l, t) ->
    [tick; Label.pp l; break_1; pp t] |> concat |> egyptian parens 2
  | `Lam (v, t) ->
    [lambda_lower; Var.pp v; dot; pp t] |> concat |> egyptian parens 2
  | `Mu t -> [mu_lower; pp t |> egyptian parens 2] |> concat
  | `Product ls ->
    ls
    |> List.map (fun (l, t) -> [Label.pp l; equals; pp t] |> concat)
    |> separate comma_break_1 |> egyptian braces 2
  | `Select (t, `Inject (l, `Product [])) -> [pp t; dot; Label.pp l] |> concat
  | `Select (t, l) -> [pp t; dot; pp l |> egyptian parens 2] |> concat
  | `Var v -> Var.pp v

let[@warning "-32"] to_string = pp >>> FomPP.to_string

(* *)

let is_mu = function `Mu _ -> true | _ -> false

let rec is_lam_or_case = function
  | `Lam _ -> true
  | `Case (`Product fs) -> List.for_all (snd >>> is_lam_or_case) fs
  | _ -> false

let is_const = function `Const _ -> true | _ -> false
