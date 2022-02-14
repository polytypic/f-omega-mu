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
    | `OpArithMinus, `Const (`Nat v) -> Some (`Const (`Nat (Int32.neg v)))
    | `OpArithMinus, `App (`Const `OpArithMinus, x) -> Some x
    (* ! *)
    | `OpLogicalNot, `App (`Const `OpLogicalNot, x) -> Some x
    | `OpLogicalNot, `Const (`Bool v) -> Some (`Const (`Bool (not v)))
    | _ -> None

  (* TODO: More comprehensive constant folding rules *)
  let simplify_bop =
    let some x = return @@ Some x
    and if_total e x =
      let+ e_total = Lam.is_total e in
      if e_total then Some x else None
    in
    function
    (* + *)
    | `OpArithAdd, `Const (`Nat x), `Const (`Nat y) ->
      some @@ `Const (`Nat (Int32.add x y))
    | ( `OpArithAdd,
        `App (`App (`Const `OpArithAdd, x), `Const (`Nat y)),
        `Const (`Nat z) ) ->
      some (`App (`App (`Const `OpArithAdd, x), `Const (`Nat (Int32.add y z))))
    | `OpArithAdd, (`Const _ as c), x ->
      some @@ `App (`App (`Const `OpArithAdd, x), c)
    | `OpArithAdd, `App (`App (`Const `OpArithAdd, x), (`Const _ as c)), y ->
      some
        (`App
          (`App (`Const `OpArithAdd, `App (`App (`Const `OpArithAdd, x), y)), c))
    | `OpArithAdd, x, `Const (`Nat 0l) -> some x
    | `OpArithAdd, x, `App (`Const `OpArithMinus, y) ->
      some @@ `App (`App (`Const `OpArithSub, x), y)
    | `OpArithAdd, `Var i, `Var j when Var.equal i j ->
      some @@ `App (`App (`Const `OpArithMul, `Const (`Nat 2l)), `Var i)
    | `OpArithAdd, x, `Const (`Nat y)
      when y < Int32.of_int 0 && y <> Int32.min_int ->
      some @@ `App (`App (`Const `OpArithSub, x), `Const (`Nat (Int32.neg y)))
    (* / *)
    | `OpArithDiv, e, `Const (`Nat 0l) | `OpArithDiv, `Const (`Nat 0l), e ->
      if_total e @@ `Const (`Nat 0l)
    | `OpArithDiv, `Const (`Nat x), `Const (`Nat y) ->
      some @@ `Const (`Nat (Int32.div x y))
    | `OpArithDiv, x, `Const (`Nat 1l) -> some x
    | `OpArithDiv, x, `Const (`Nat -1l) -> some @@ `App (`Const `OpArithMinus, x)
    (* * *)
    | `OpArithMul, `Const (`Nat x), `Const (`Nat y) ->
      some @@ `Const (`Nat (Int32.mul x y))
    | `OpArithMul, x, `Const (`Nat 1l) | `OpArithMul, `Const (`Nat 1l), x ->
      some x
    | `OpArithMul, x, `Const (`Nat -1l) | `OpArithMul, `Const (`Nat -1l), x ->
      some @@ `App (`Const `OpArithMinus, x)
    | `OpArithMul, e, `Const (`Nat 0l) | `OpArithMul, `Const (`Nat 0l), e ->
      if_total e @@ `Const (`Nat 0l)
    (* - *)
    | `OpArithSub, `Var i, `Var j when Var.equal i j -> some @@ `Const (`Nat 0l)
    | `OpArithSub, `Const (`Nat x), `Const (`Nat y) ->
      some @@ `Const (`Nat (Int32.sub x y))
    | `OpArithSub, x, `Const (`Nat 0l) -> some x
    | `OpArithSub, `Const (`Nat 0l), x -> some @@ `App (`Const `OpArithMinus, x)
    | `OpArithSub, x, `App (`Const `OpArithMinus, y) ->
      some @@ `App (`App (`Const `OpArithAdd, x), y)
    (* *)
    | `OpStringCat, `Const (`String l), `Const (`String r) ->
      some
        (`Const
          (`String
            (Stdlib.( ^ ) (JsonString.to_utf8 l) (JsonString.to_utf8 r)
            |> JsonString.of_utf8)))
    | `OpStringCat, x, `Const (`String empty)
    | `OpStringCat, `Const (`String empty), x
      when JsonString.is_empty empty ->
      some x
    (* *)
    | `OpEq _, `Const x, `Const y -> some @@ `Const (`Bool (compare x y = 0))
    | `OpEqNot _, `Const x, `Const y ->
      some @@ `Const (`Bool (compare x y <> 0))
    (* *)
    | `OpCmpLt, `Const (`Nat x), `Const (`Nat y)
    | `OpCmpGt, `Const (`Nat y), `Const (`Nat x) ->
      some @@ `Const (`Bool (Int32.compare x y < 0))
    | `OpCmpLtEq, `Const (`Nat x), `Const (`Nat y)
    | `OpCmpGtEq, `Const (`Nat y), `Const (`Nat x) ->
      some @@ `Const (`Bool (Int32.compare x y <= 0))
    (* *)
    | `OpLogicalAnd, x, `Const (`Bool true)
    | `OpLogicalAnd, `Const (`Bool true), x
    | `OpLogicalOr, x, `Const (`Bool false)
    | `OpLogicalOr, `Const (`Bool false), x ->
      some x
    | `OpLogicalOr, `Const (`Bool true), _ -> some @@ `Const (`Bool true)
    | `OpLogicalOr, lhs, `Const (`Bool true) ->
      if_total lhs @@ `Const (`Bool true)
    | `OpLogicalAnd, `Const (`Bool false), _ -> some @@ `Const (`Bool false)
    | `OpLogicalAnd, lhs, `Const (`Bool false) ->
      if_total lhs @@ `Const (`Bool false)
    | (`OpLogicalOr | `OpLogicalAnd), `Var x, `Var y when Var.equal x y ->
      some @@ `Var x
    (* *)
    | _ -> return None
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
    &&& thunk (fun () -> (not once) || List.for_all (is_free i' >>> not) xs)
    ||| (thunk (fun () -> (not once) || not (is_free i' x))
        &&& is_total x
        &&& occurs_in_total_position_of_list ~once i' xs)

let to_lam continue k i e =
  let i, e =
    if is_free i k then
      let i' = Var.freshen i in
      let vi' = `Var i' in
      (i', subst i vi' e)
    else (i, e)
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
  if Seen.mem e seen then fail `Seen
  else
    let* limit = get Limit.field in
    if limit < size e then fail `Limit
    else Seen.adding e (simplify_base e >>- keep_phys_eq' e)

and simplify_base = function
  | `Const (`Target (_, l)) when Js.is_identity l ->
    let i = Var.of_string Loc.dummy "x" |> Var.freshen in
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
        let+ e = simplify e |> Env.adding i x in
        keep_phys_eq' f @@ `Lam (i, e)
      | _ -> simplify f
    in
    let* f_is_total = is_total f in
    let default () = return @@ `App (f, x) in
    match (f, x) with
    | f, `App (`Lam (i, x), v) when f_is_total && is_strict f ->
      let i' = Var.freshen i in
      let x = subst i (`Var i') x in
      let+ f_x = simplify @@ `App (f, x) |> Env.adding i' v in
      `App (`Lam (i', f_x), v)
    | `Case (`Product _), s when f_is_total && may_inline_continuation s ->
      let+ inlined =
        simplify (inline_continuation s (lam @@ fun s -> `App (f, s)))
      and+ defaulted = default () in
      if size inlined * 3 < size defaulted * 4 then inlined else defaulted
    | `Case (`Product fs), `Inject (l, e) when f_is_total ->
      simplify @@ `App (List.find (fst >>> Label.equal l) fs |> snd, e)
    | `Const c, x when Const.is_uop c -> (
      match Const.simplify_uop (c, x) with
      | Some e -> simplify e
      | None -> default ())
    | `App (`Const c, x), y when Const.is_bop c -> (
      Const.simplify_bop (c, x, y) >>= function
      | Some e -> simplify e
      | None -> default ())
    | `Lam (i, e), x -> (
      let* defaulted = default () in
      let apply () =
        simplify (subst i x e) |> mapping Limit.field (fun v -> v / 16 * 15)
      in
      let* may_subst =
        is_total x
        &&& thunk (fun () -> tag x <> `Mu || not (is_free i e))
        ||| occurs_in_total_position ~once:true i e
      in
      if may_subst then
        apply ()
        |> try_in
             (fun applied ->
               if
                 size applied * 3 < size defaulted * 4
                 && Lam.compare applied defaulted <> 0
               then return applied
               else return defaulted)
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
      else default ()
    | `IfElse (c, `Lam (t', t), `Lam (e', e)), x ->
      let* c_is_total = is_total c in
      if c_is_total then
        let x' = Var.of_string Loc.dummy "_IfElse" |> Var.freshen in
        let xv = `Var x' in
        simplify
        @@ `App (`Lam (x', `IfElse (c, subst t' xv t, subst e' xv e)), x)
      else default ()
    | `Var _, c when may_inline_continuation c ->
      simplify @@ inline_continuation c f
    | _ -> default ())
  | `Mu (`Lam (f, e) as lam) -> (
    match unlam e with
    | is, `Case (`Product fs)
      when List.for_all (snd >>> always_applied_to_inject f) fs ->
      let i = Var.of_string Loc.dummy "_Case_i" |> Var.freshen in
      let v = Var.of_string Loc.dummy "_Case_v" |> Var.freshen in
      let unit = `Const `Unit in
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
      let+ e = simplify e |> Env.adding f e in
      if is_free f e then `Mu (keep_phys_eq' lam @@ `Lam (f, e)) else e)
  | `Mu e -> (
    let+ e = simplify e in
    match e with `Lam (i, e) when not (is_free i e) -> e | e -> `Mu e)
  | `IfElse (`App (`Const `OpLogicalNot, c), t, e) ->
    simplify @@ `IfElse (c, e, t)
  | `IfElse (c, t, e) -> (
    let* c = simplify c in
    match c with
    | `Const (`Bool c) -> simplify (if c then t else e)
    | `App (`Lam (i, b), v) ->
      let i' = Var.freshen i in
      let+ c = simplify @@ `IfElse (subst i (`Var i') b, t, e) in
      `App (`Lam (i', c), v)
    | _ -> (
      simplify t <*> simplify e >>= function
      | `Const (`Bool true), e ->
        simplify @@ `App (`App (`Const `OpLogicalOr, c), e)
      | t, `Const (`Bool false) ->
        simplify @@ `App (`App (`Const `OpLogicalAnd, c), t)
      | `Const (`Bool false), `Const (`Bool true) ->
        return @@ `App (`Const `OpLogicalNot, c)
      | t, e ->
        let default () = return @@ `IfElse (c, t, e) in
        if Lam.compare t e = 0 then
          is_total c >>- function
          | true -> t
          | _ -> `App (`Lam (Var.of_string Loc.dummy "_If" |> Var.freshen, t), c)
        else if Lam.compare c t = 0 then
          is_total c >>= function
          | true -> simplify @@ `App (`App (`Const `OpLogicalOr, c), e)
          | false -> default ()
        else if Lam.compare c e = 0 then
          is_total c >>= function
          | true -> simplify @@ `App (`App (`Const `OpLogicalAnd, c), t)
          | false -> default ()
        else default ()))
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
      else default ()
    | _ -> default ())
  | `Case cs -> simplify cs >>- fun cs -> `Case cs
  | `Inject (l, e) -> (
    simplify e >>- function
    | `App (`Lam (i, b), v) -> `App (`Lam (i, `Inject (l, b)), v)
    | e -> `Inject (l, e))

let once e = simplify e |> try_in return @@ fun (`Limit | `Seen) -> return e

let rec to_fixed_point e =
  let* limit = get Limit.field in
  if limit < size e then return e
  else
    let* e' = once e in
    if e == e' then return e'
    else to_fixed_point e' |> mapping Limit.field (fun v -> v / 16 * 15)

let to_fixed_point e = to_fixed_point e |> setting Limit.field (size e * 8)
let once e = once e |> setting Limit.field (size e * 4)
