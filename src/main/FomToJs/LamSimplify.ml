open FomBasis
open FomSource
open FomAST

(* *)

open Lam

module Const = struct
  include Const

  let simplify_uop = function
    (* + *)
    | `OpArithPlus, x -> Some x
    (* - *)
    | `OpArithMinus, `Const (`LitNat v) -> Some (`Const (`LitNat (Int32.neg v)))
    | `OpArithMinus, `App (`Const `OpArithMinus, x) -> Some x
    (* ! *)
    | `OpLogicalNot, `App (`Const `OpLogicalNot, x) -> Some x
    | `OpLogicalNot, `Const (`LitBool v) -> Some (`Const (`LitBool (not v)))
    | _ -> None

  (* TODO: More comprehensive constant folding rules *)
  let simplify_bop = function
    (* + *)
    | `OpArithAdd, `Const (`LitNat x), `Const (`LitNat y) ->
      Some (`Const (`LitNat (Int32.add x y)))
    | ( `OpArithAdd,
        `App (`App (`Const `OpArithAdd, x), `Const (`LitNat y)),
        `Const (`LitNat z) ) ->
      Some
        (`App (`App (`Const `OpArithAdd, x), `Const (`LitNat (Int32.add y z))))
    | `OpArithAdd, (`Const _ as c), x ->
      Some (`App (`App (`Const `OpArithAdd, x), c))
    | `OpArithAdd, `App (`App (`Const `OpArithAdd, x), (`Const _ as c)), y ->
      Some
        (`App
          (`App (`Const `OpArithAdd, `App (`App (`Const `OpArithAdd, x), y)), c))
    | `OpArithAdd, x, `Const (`LitNat 0l) -> Some x
    | `OpArithAdd, x, `App (`Const `OpArithMinus, y) ->
      Some (`App (`App (`Const `OpArithSub, x), y))
    | `OpArithAdd, `Var i, `Var j when Var.equal i j ->
      Some (`App (`App (`Const `OpArithMul, `Const (`LitNat 2l)), `Var i))
    | `OpArithAdd, x, `Const (`LitNat y)
      when y < Int32.of_int 0 && y <> Int32.min_int ->
      Some (`App (`App (`Const `OpArithSub, x), `Const (`LitNat (Int32.neg y))))
    (* / *)
    | `OpArithDiv, _, `Const (`LitNat 0l) | `OpArithDiv, `Const (`LitNat 0l), _
      ->
      Some (`Const (`LitNat 0l))
    | `OpArithDiv, `Const (`LitNat x), `Const (`LitNat y) ->
      Some (`Const (`LitNat (Int32.div x y)))
    | `OpArithDiv, x, `Const (`LitNat 1l) -> Some x
    | `OpArithDiv, x, `Const (`LitNat -1l) ->
      Some (`App (`Const `OpArithMinus, x))
    (* * *)
    | `OpArithMul, `Const (`LitNat x), `Const (`LitNat y) ->
      Some (`Const (`LitNat (Int32.mul x y)))
    | `OpArithMul, x, `Const (`LitNat 1l) | `OpArithMul, `Const (`LitNat 1l), x
      ->
      Some x
    | `OpArithMul, x, `Const (`LitNat -1l)
    | `OpArithMul, `Const (`LitNat -1l), x ->
      Some (`App (`Const `OpArithMinus, x))
    | `OpArithMul, _, `Const (`LitNat 0l) | `OpArithMul, `Const (`LitNat 0l), _
      ->
      Some (`Const (`LitNat 0l))
    (* - *)
    | `OpArithSub, `Var i, `Var j when Var.equal i j ->
      Some (`Const (`LitNat 0l))
    | `OpArithSub, `Const (`LitNat x), `Const (`LitNat y) ->
      Some (`Const (`LitNat (Int32.sub x y)))
    | `OpArithSub, x, `Const (`LitNat 0l) -> Some x
    | `OpArithSub, `Const (`LitNat 0l), x ->
      Some (`App (`Const `OpArithMinus, x))
    | `OpArithSub, x, `App (`Const `OpArithMinus, y) ->
      Some (`App (`App (`Const `OpArithAdd, x), y))
    (* *)
    | `OpStringCat, `Const (`LitString l), `Const (`LitString r) ->
      Some
        (`Const
          (`LitString
            (Stdlib.( ^ ) (JsonString.to_utf8 l) (JsonString.to_utf8 r)
            |> JsonString.of_utf8)))
    | `OpStringCat, x, `Const (`LitString empty)
    | `OpStringCat, `Const (`LitString empty), x
      when JsonString.is_empty empty ->
      Some x
    (* *)
    | `OpEq _, `Const x, `Const y -> Some (`Const (`LitBool (compare x y = 0)))
    | `OpEqNot _, `Const x, `Const y ->
      Some (`Const (`LitBool (compare x y <> 0)))
    (* *)
    | `OpCmpLt, `Const (`LitNat x), `Const (`LitNat y)
    | `OpCmpGt, `Const (`LitNat y), `Const (`LitNat x) ->
      Some (`Const (`LitBool (Int32.compare x y < 0)))
    | `OpCmpLtEq, `Const (`LitNat x), `Const (`LitNat y)
    | `OpCmpGtEq, `Const (`LitNat y), `Const (`LitNat x) ->
      Some (`Const (`LitBool (Int32.compare x y <= 0)))
    (* *)
    | `OpLogicalAnd, x, `Const (`LitBool true)
    | `OpLogicalAnd, `Const (`LitBool true), x
    | `OpLogicalOr, x, `Const (`LitBool false)
    | `OpLogicalOr, `Const (`LitBool false), x ->
      Some x
    | `OpLogicalOr, `Const (`LitBool true), _
    | `OpLogicalOr, `Var _, `Const (`LitBool true) ->
      Some (`Const (`LitBool true))
    | `OpLogicalAnd, `Const (`LitBool false), _
    | `OpLogicalAnd, `Var _, `Const (`LitBool false) ->
      Some (`Const (`LitBool false))
    | (`OpLogicalOr | `OpLogicalAnd), `Var x, `Var y when Var.equal x y ->
      Some (`Var x)
    (* *)
    | _ -> None
end

let rec always_applied_to_inject i' e =
  let f, xs = unapp e in
  List.for_all (always_applied_to_inject i') xs
  &&
  match f with
  | `Const _ -> true
  | `Var i -> (
    (not (Var.equal i' i))
    || match List.rev xs with `Inject _ :: _ -> true | _ -> false)
  | `Lam (i, e) -> Var.equal i' i || always_applied_to_inject i' e
  | `App _ -> failwith "always_applied_to_inject"
  | `IfElse (c, t, e) ->
    always_applied_to_inject i' c
    && always_applied_to_inject i' t
    && always_applied_to_inject i' e
  | `Product fs -> fs |> List.for_all (snd >>> always_applied_to_inject i')
  | `Mu e | `Inject (_, e) | `Case e -> always_applied_to_inject i' e
  | `Select (e, l) ->
    always_applied_to_inject i' e && always_applied_to_inject i' l

let rec occurs_in_total_position ~once i' e =
  match unapp e with
  | `Var i, [] -> return @@ Var.equal i' i
  | `Const c, xs when Const.is_total c ->
    occurs_in_total_position_of_list ~once i' xs
  | f, xs when is_lam_or_case f && ((not once) || not (is_free i' f)) ->
    occurs_in_total_position_of_list ~once i' xs
  | _ -> return false

and occurs_in_total_position_of_list ~once i' = function
  | [] -> return false
  | x :: xs ->
    occurs_in_total_position ~once i' x
    &&& return ((not once) || List.for_all (is_free i' >>> not) xs)
    ||| (return ((not once) || not (is_free i' x))
        &&& is_total x
        &&& occurs_in_total_position_of_list ~once i' xs)

let to_lam continue k i e =
  let i, e =
    if is_free i k then
      let i' = Var.freshen i in
      let vi' = `Var i' in
      (i', subst i vi' e)
    else
      (i, e)
  in
  `Lam (i, continue e k)

let rec to_case continue k fs =
  fs
  |> Row.map (function
       | `Lam (i, e) -> to_lam continue k i e
       | `Case (`Product fs) -> to_case continue k fs
       | _ -> failwith "to_case")
  |> fun fs -> `Case (`Product fs)

let may_inline_continuation = function
  | `IfElse _ -> true
  | `App (f, _) when is_lam_or_case f -> true
  | _ -> false

let rec inline_continuation e k =
  match e with
  | `IfElse (c, t, e) ->
    `IfElse (c, inline_continuation t k, inline_continuation e k)
  | `App (f, x) when is_lam_or_case f -> (
    match f with
    | `Lam (i, e) -> `App (to_lam inline_continuation k i e, x)
    | `Case (`Product fs) -> `App (to_case inline_continuation k fs, x)
    | _ -> failwith "inline_continuation")
  | ( `Const _ | `Var _ | `Lam _ | `Mu _ | `Product _ | `Select _ | `Inject _
    | `App _ | `Case _ ) as e ->
    `App (k, e)

let rec simplify e =
  let* seen = get Seen.field in
  if Seen.mem e seen then
    fail `Seen
  else
    let* limit = get Limit.field in
    if limit < size e then
      fail `Limit
    else
      Seen.adding e (simplify_base e >>- keep_phys_eq' e)

and simplify_base = function
  | `Const (`Target (_, l)) when Js.is_identity l ->
    let i = Var.fresh Loc.dummy in
    return @@ `Lam (i, `Var i)
  | (`Const _ | `Var _) as e -> return e
  | `Lam (i, `Lam (j, `App (`App (`Const c, `Var y), `Var x)))
    when Var.equal i x && Var.equal j y && Const.is_commutative c ->
    return @@ `Const c
  | `Lam (i, e) -> (
    let* e = simplify e in
    let default () = return @@ `Lam (i, e) in
    match e with
    | `App (f, `Var i') when Var.equal i i' && not (is_free i f) ->
      let* f_is_total = is_total f in
      if f_is_total then return f else default ()
    | _ -> default ())
  | `App (f, x) -> (
    let* x = simplify x in
    let* f =
      match f with
      | `Lam (i, e) ->
        let+ e = simplify e |> VarMap.adding i x in
        keep_phys_eq' f @@ `Lam (i, e)
      | _ -> simplify f
    in
    let default () = return @@ `App (f, x) in
    match (f, x) with
    | `Case cs, s ->
      let* cs_is_total = is_total cs in
      if cs_is_total then
        match (s, cs) with
        | `Inject (l, e), `Product fs ->
          simplify @@ `App (List.find (fst >>> Label.equal l) fs |> snd, e)
        | _, `Product _ when may_inline_continuation s ->
          let+ inlined =
            simplify
              (inline_continuation s (lam @@ fun s -> `App (`Case cs, s)))
          and+ defaulted = default () in
          if size inlined * 3 < size defaulted * 4 then
            inlined
          else
            defaulted
        | _ -> default ()
      else
        default ()
    | `Const c, x when Const.is_uop c -> (
      match Const.simplify_uop (c, x) with
      | Some e -> simplify e
      | None -> default ())
    | `App (`Const c, x), y when Const.is_bop c -> (
      match Const.simplify_bop (c, x, y) with
      | Some e -> simplify e
      | None -> default ())
    | `Lam (i, e), `App (`Lam (j, f), y) ->
      let j', f' =
        if is_free j e || Var.equal i j then
          let j' = Var.freshen j in
          let vj' = `Var j' in
          (j', subst j vj' f)
        else
          (j, f)
      in
      simplify @@ `App (`Lam (j', `App (`Lam (i, e), f')), y)
    | `Lam (i, `Var i'), x when Var.equal i i' -> return x
    | `Lam (i, e), x -> (
      let* defaulted = default () in
      let apply () =
        simplify (subst i x e) |> mapping Limit.field (fun v -> v / 16 * 15)
      in
      let* may_subst =
        is_total x
        &&& return ((not (is_mu x)) || not (is_free i e))
        ||| occurs_in_total_position ~once:true i e
      in
      if may_subst then
        apply ()
        |> try_in
             (fun applied ->
               if
                 size applied * 3 < size defaulted * 4
                 && Lam.compare applied defaulted <> 0
               then
                 return applied
               else
                 return defaulted)
             (fun (`Limit | `Seen) -> return defaulted)
      else
        match x with
        | `Product fs ->
          fs |> List.rev
          |> List.fold_left
               (fun e (l, v) -> `App (`Lam (Var.of_label l, e), v))
               ( fs |> List.map (fun (l, _) -> (l, `Var (Var.of_label l)))
               |> fun fs -> `App (`Lam (i, e), `Product fs) )
          |> simplify
        | `Inject (l, v) ->
          let i = Var.of_label l in
          `App (`Lam (i, `App (f, `Inject (l, `Var i))), v) |> simplify
        | _ -> return defaulted)
    | `App (`Lam (x', `Lam (y', e)), x), y ->
      let x'' = Var.freshen x' in
      simplify
      @@ `App (`Lam (x'', `App (`Lam (y', subst x' (`Var x'') e), y)), x)
    | `App (`Lam (x', e), x), y ->
      let* e_or_y_is_total = is_total e ||| is_total y in
      if e_or_y_is_total then
        let x'' = Var.freshen x' in
        simplify @@ `App (`Lam (x'', `App (subst x' (`Var x'') e, y)), x)
      else
        default ()
    | `IfElse (c, `Lam (t', t), `Lam (e', e)), x ->
      let* c_is_total = is_total c in
      if c_is_total then
        let x' = Var.fresh Loc.dummy in
        let xv = `Var x' in
        simplify
        @@ `App (`Lam (x', `IfElse (c, subst t' xv t, subst e' xv e)), x)
      else
        default ()
    | `Var _, c when may_inline_continuation c ->
      simplify @@ inline_continuation c f
    | _ -> default ())
  | `Mu (`Lam (f, e) as lam) -> (
    match unlam e with
    | is, `Case (`Product fs)
      when List.for_all (snd >>> always_applied_to_inject f) fs ->
      let i = Var.fresh Loc.dummy in
      let v = Var.fresh Loc.dummy in
      let unit = `Product [] in
      let fn =
        fs
        |> List.map (fun (l, _) ->
               ( l,
                 `Lam
                   ( v,
                     `App
                       ( apps
                           (`Select (`Var i, `Inject (l, unit)))
                           (is |> List.map (fun i -> `Var i)),
                         `Var v ) ) ))
        |> fun fs -> lams is @@ `Case (`Product fs)
      in
      fs |> Row.map (fun v -> `App (`Lam (f, v), fn)) |> fun fs ->
      `App (`Lam (i, fn), `Mu (`Lam (i, `Product (fs |> Row.map (lams is)))))
      |> simplify
    | _ ->
      let+ e = simplify e |> VarMap.adding f e in
      if is_free f e then `Mu (keep_phys_eq' lam @@ `Lam (f, e)) else e)
  | `Mu e -> (
    let+ e = simplify e in
    match e with `Lam (i, e) when not (is_free i e) -> e | e -> `Mu e)
  | `IfElse (`App (`Const `OpLogicalNot, c), t, e) ->
    simplify @@ `IfElse (c, e, t)
  | `IfElse (c, t, e) -> (
    let* c = simplify c in
    match c with
    | `Const (`LitBool c) -> simplify (if c then t else e)
    | _ -> (
      simplify t <*> simplify e >>= function
      | `Const (`LitBool true), e ->
        simplify @@ `App (`App (`Const `OpLogicalOr, c), e)
      | t, `Const (`LitBool false) ->
        simplify @@ `App (`App (`Const `OpLogicalAnd, c), t)
      | `Const (`LitBool false), `Const (`LitBool true) ->
        return @@ `App (`Const `OpLogicalNot, c)
      | t, e ->
        let default () = return @@ `IfElse (c, t, e) in
        if Lam.compare t e = 0 then
          is_total c >>- function
          | true -> t
          | _ -> `App (`Lam (Var.fresh Loc.dummy, t), c)
        else if Lam.compare c t = 0 then
          is_total c >>= function
          | true -> simplify @@ `App (`App (`Const `OpLogicalOr, c), e)
          | false -> default ()
        else if Lam.compare c e = 0 then
          is_total c >>= function
          | true -> simplify @@ `App (`App (`Const `OpLogicalAnd, c), t)
          | false -> default ()
        else
          default ()))
  | `Product fs -> Row.map_phys_eq_fr simplify fs >>- fun fs -> `Product fs
  | `Select (e, l) -> (
    let* e = simplify e and* l = simplify l in
    let default () = return @@ `Select (e, l) in
    match (e, l) with
    | `Product fs, `Inject (l, _) ->
      let* fs_are_total =
        fs
        |> List.filter (fst >>> Label.equal l >>> not)
        |> List.for_all_fr (snd >>> is_total)
      in
      if fs_are_total then
        fs |> List.find (fst >>> Label.equal l) |> snd |> return
      else
        default ()
    | _ -> default ())
  | `Case cs -> simplify cs >>- fun cs -> `Case cs
  | `Inject (l, e) -> simplify e >>- fun e -> `Inject (l, e)

let once e = simplify e |> try_in return @@ fun (`Limit | `Seen) -> return e

let rec to_fixed_point e =
  let* limit = get Limit.field in
  if limit < size e then
    return e
  else
    let* e' = once e in
    if e == e' then
      return e'
    else
      to_fixed_point e' |> mapping Limit.field (fun v -> v / 16 * 15)

let to_fixed_point e = to_fixed_point e |> setting Limit.field (size e * 8)
let once e = once e |> setting Limit.field (size e * 4)
