open FomBasis
open FomPPrint
open FomParser
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
    | `Bool _ | `Nat _ | `String _ | `Unit | `OpArithAdd | `OpArithDiv
    | `OpArithMul | `OpArithRem | `OpArithSub | `OpCmpGt | `OpCmpGtEq | `OpCmpLt
    | `OpCmpLtEq | `OpEq _ | `OpEqNot _ | `OpLogicalAnd | `OpLogicalOr
    | `OpStringCat | `Target _ ->
      false

  let is_bop = function
    | `Bool _ | `Nat _ | `String _ | `Unit | `Target _ | `OpArithMinus
    | `OpArithPlus | `OpLogicalNot | `Keep _ ->
      false
    | `OpArithAdd | `OpArithDiv | `OpArithMul | `OpArithRem | `OpArithSub
    | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpEq _ | `OpEqNot _
    | `OpLogicalAnd | `OpLogicalOr | `OpStringCat ->
      true

  let is_commutative = function
    | `OpArithAdd | `OpArithMul | `OpEq _ | `OpEqNot _ -> true
    | `Bool _ | `Nat _ | `String _ | `Unit | `OpArithDiv | `OpArithMinus
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

let map_fr' row fn = function
  | `App (f, x) -> fn f <*> fn x >>- fun f_x -> `App f_x
  | `Case e -> fn e >>- fun e -> `Case e
  | `Const _ as inn -> return inn
  | `IfElse (c, t, e) -> tuple'3 (fn c) (fn t) (fn e) >>- fun x -> `IfElse x
  | `Inject (l, e) -> fn e >>- fun e -> `Inject (l, e)
  | `Lam (i, e) -> fn e >>- fun e -> `Lam (i, e)
  | `Mu e -> fn e >>- fun e -> `Mu e
  | `Product fs -> row fn fs >>- fun fs -> `Product fs
  | `Select (e, l) -> fn e <*> fn l >>- fun e_l -> `Select e_l
  | `Var _ as inn -> return inn

let map_fr fn = map_fr' Row.map_fr fn
let map_eq_fr fn = map_fr' Row.map_phys_eq_fr fn
let map fn = Traverse.to_map map_fr fn
let map_eq fn = Traverse.to_map map_eq_fr fn
let map_constant m = Traverse.to_map_constant map_fr m
let exists fn = Traverse.to_exists map_fr fn
let find_map fn = Traverse.to_find_map map_fr fn
let map_reduce plus = Traverse.to_map_reduce map_fr plus

(* *)

let to_case_param = function
  | `Product ls ->
    ls
    |> List.map (fst >>> Label.to_string >>> String.cat "'")
    |> String.concat " | " |> Lexer.coerce_to_id
  | _ -> "_case"

(* *)

let lam fn =
  let i = Var.of_string Loc.dummy "_lam" |> Var.freshen in
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
  | `Case cs as e ->
    let i = cs |> to_case_param |> Var.of_string Loc.dummy |> Var.freshen in
    (is @ [i], `App (e, `Var i))
  | _ -> (is, e)

let lams is = List.fold_right (fun i e -> `Lam (i, e)) is

(* *)

let rec is_free i = function
  | `Var i' -> Var.equal i i'
  | `Lam (i', e) -> (not (Var.equal i i')) && is_free i e
  | e -> exists (is_free i) e

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
  | e -> map_eq (subst_par env) e

let subst i = function
  | `Var j when Var.equal i j -> id
  | the -> subst_par (VarMap.singleton i the)

(* *)

let rec size t = map_reduce ( + ) 0 size t + 1

(* *)

let tag = function
  | `App _ -> `App
  | `Case _ -> `Case
  | `Const _ -> `Const
  | `IfElse _ -> `IfElse
  | `Inject _ -> `Inject
  | `Lam _ -> `Lam
  | `Mu _ -> `Mu
  | `Product _ -> `Product
  | `Select _ -> `Select
  | `Var _ -> `Var

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
    | _ -> Stdlib.compare (tag l) (tag r)

(* *)

let const_pp c =
  FomPP.Exp.Const.pp' (Int32.to_string >>> utf8string) FomPP.Typ.pp c

let seems_atomic cs =
  let n = Array.length cs in
  let rec loop p i =
    n <= i
    ||
    match Uchar.to_int cs.(i) with
    | 0x16a47 | 0x1bc19 | 0x1bc1d -> loop (p + 1) (i + 1)
    | 0x16a49 | 0x1bc1a | 0x1bc1e -> 0 < p && loop (p - 1) (i + 1)
    | 0x5f | 0x03bc | 0x03bb -> 0 < p && loop p (i + 1)
    | _ -> loop p (i + 1)
  in
  loop 0 0

let seems_parenthesized cs =
  0 < Array.length cs
  &&
  match Uchar.to_int cs.(0) with
  | 0x16a47 | 0x1bc19 | 0x1bc1d -> seems_atomic cs
  | _ -> false

let rec pp atom : t -> document = function
  | `App (`App (`Const c, x), y) when Const.is_bop c ->
    pp true x ^^ space ^^ const_pp c ^^ space ^^ pp true y
    |> if atom then egyptian parens 2 else id
  | `App (`Const c, x) when Const.is_uop c ->
    const_pp c ^^ pp true x |> if atom then egyptian parens 2 else id
  | `App (f, x) ->
    pp true f ^^ space ^^ pp true x |> if atom then egyptian parens 2 else id
  | `Case t -> case' ^^ space ^^ pp true t
  | `Const c -> const_pp c
  | `IfElse (c, t, e) ->
    if' ^^ space ^^ pp false c ^^ space ^^ then' ^^ space ^^ pp false t ^^ space
    ^^ else' ^^ space ^^ pp false e
    |> if atom then egyptian parens 2 else id
  | `Inject (l, `Var v) ->
    let d = Var.pp v in
    tick ^^ Label.pp l
    ^^
    if d |> to_string |> UTF.UTF8.to_uchar_array |> seems_parenthesized then d
    else egyptian parens 2 d
  | `Inject (l, `Const `Unit) -> tick ^^ Label.pp l
  | `Inject (l, (`Product _ as e)) -> tick ^^ Label.pp l ^^ pp true e
  | `Inject (l, t) -> tick ^^ Label.pp l ^^ egyptian parens 2 (pp false t)
  | `Lam (v, t) ->
    lambda_lower ^^ Var.pp v ^^ dot ^^ pp false t
    |> if atom then egyptian parens 2 else id
  | `Mu (`Lam (v, t)) ->
    mu_lower ^^ Var.pp v ^^ dot ^^ pp false t
    |> if atom then egyptian parens 2 else id
  | `Mu t -> mu_lower ^^ pp true t
  | `Product ls ->
    if Row.is_tuple ls then
      ls
      |> List.map (snd >>> pp false)
      |> separate comma_break_1 |> egyptian parens 2
    else
      ls
      |> List.map (fun (l, t) -> Label.pp l ^^ equals ^^ pp false t)
      |> separate comma_break_1 |> egyptian braces 2
  | `Select (t, `Inject (l, `Const `Unit)) -> pp true t ^^ dot ^^ Label.pp l
  | `Select (t, l) -> pp true t ^^ dot ^^ pp true l
  | `Var v ->
    let d = Var.pp v in
    if atom && d |> to_string |> UTF.UTF8.to_uchar_array |> seems_atomic |> not
    then egyptian parens 2 d
    else d

let to_string = pp false >>> to_string

(* *)

let rec is_lam_or_case = function
  | `Lam _ -> true
  | `Case (`Product fs) -> List.for_all (snd >>> is_lam_or_case) fs
  | _ -> false
