open Rea
open StdlibPlus
open FomSource

(* *)

include LamCore

module Env = struct
  include VarMap

  type m = LamCore.t t Prop.t

  let field r : m = r#env
  let find_opt i = get_as field (find_opt i)
  let adding i e = mapping field (add i e)

  class con =
    object
      val mutable env = empty
      method env : m = prop (fun () -> env) (fun x -> env <- x)
    end
end

module Limit = struct
  type m = int Prop.t

  let field r : m = r#limit

  class con =
    object
      val mutable limit = 0
      method limit = prop (fun () -> limit) (fun x -> limit <- x)
    end
end

module Seen = struct
  include Set.Make (Compare.Tuple'2 (Int) (LamCore))

  type m = t Prop.t

  let field r : m = r#seen

  let adding e on =
    let* old_seen = get field in
    let k = (hash e, e) in
    let new_seen = add k old_seen in
    if old_seen == new_seen then on `Old else setting field new_seen (on `New)

  class con =
    object
      val mutable seen = empty
      method seen = prop (fun () -> seen) (fun x -> seen <- x)
    end
end

module Renumbering = struct
  include Map.Make (Id.Name)

  type m = int t Prop.t

  let field r : m = r#renumbering

  class con =
    object
      val mutable renum = empty
      method renumbering : m = prop (fun () -> renum) (fun x -> renum <- x)
    end

  let fresh_of_name at name inn =
    let* renumbering = get field in
    let n =
      match find_opt name renumbering with
      | None -> if Id.Name.fresh = name then 1 else 0
      | Some n -> n + 1
    in
    inn (Var.Unsafe.set_counter n (Var.of_name at name))
    |> mapping field (add name n)

  let fresh inn = fresh_of_name Loc.dummy Id.Name.fresh inn
  let freshen i = fresh_of_name (Var.at i) (Var.name i)
  let fresh_of_string s = freshen (Var.of_string Loc.dummy s)

  let freshen_all is inn =
    let rec loop i's = function
      | [] -> inn (List.rev i's)
      | i :: is -> freshen i @@ fun i' -> loop (i' :: i's) is
    in
    loop [] is

  let app inn exp =
    match unapp exp with
    | `Lam (i, e), xs ->
      freshen i @@ fun j ->
      (* TODO: `subst` makes this quadratic *)
      let exp' = apps (`Lam (j, subst i (`Var j) e)) xs in
      inn exp'
    | _ -> inn exp
end

module LamMap = Map.Make (LamCore)

module Const = struct
  include Const

  let is_strict = function
    | `OpLogicalAnd | `OpLogicalOr -> false
    | `Keep _ | `Bool _ | `Nat _ | `String _ | `Unit | `OpArithAdd | `OpArithDiv
    | `OpArithMinus | `OpArithMul | `OpArithPlus | `OpArithRem | `OpArithSub
    | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpEq _ | `OpEqNot _
    | `OpLogicalNot | `OpStringCat | `Target _ ->
      true

  let is_total = function
    | `Keep _ -> false
    | `Bool _ | `Nat _ | `String _ | `Unit | `OpArithAdd | `OpArithDiv
    | `OpArithMinus | `OpArithMul | `OpArithPlus | `OpArithRem | `OpArithSub
    | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpEq _ | `OpEqNot _
    | `OpLogicalAnd | `OpLogicalNot | `OpLogicalOr | `OpStringCat | `Target _ ->
      true
end

let dummy_var = `Var (Var.fresh Loc.dummy)

let rec is_total e =
  eta'0 @@ fun () ->
  match e with
  | `Const _ | `Var _ | `Lam _ -> pure true
  | _ -> (
    Seen.adding e @@ function
    | `Old -> pure false
    | `New -> (
      match unapp e with
      | (`Const _ | `Var _ | `Lam _), [] -> pure true
      | `IfElse (c, t, e), xs ->
        is_total c &&& is_total (apps t xs) &&& is_total (apps e xs)
      | `Product fs, _ -> fs |> List.for_all_er @@ fun (_, e) -> is_total e
      | `Mu (`Lam (i, e)), xs -> is_total (apps e xs) |> Env.adding i e
      | `Select (e, l), [] -> is_total e &&& is_total l
      | `Inject (_, e), _ -> is_total e
      | `Var f, xs -> (
        Env.find_opt f >>= function
        | None -> pure false
        | Some f -> is_total (apps f xs))
      | `Lam (i, e), x :: xs ->
        is_total x
        &&& (is_total e |> Env.adding i x)
        &&& (is_total (apps e xs) |> Env.adding i x)
      | `Const c, xs -> pure'1 Const.is_total c &&& List.for_all_er is_total xs
      | `Case (`Product fs), x :: xs ->
        is_total x
        &&& (fs
            |> List.for_all_er @@ fun (_, f) ->
               is_total (apps f (dummy_var :: xs)))
      | `Case e, [] -> is_total e
      | (`Mu _ | `App _ | `Select _ | `Case _), _ -> pure false))

(* *)

let is_strict e =
  match e with `App (`Const c, _) -> Const.is_strict c | _ -> true

(* *)

let rec called_at_tail n f' e =
  match unapp e with
  | `Var i, xs -> Var.equal i f' && List.length xs = n
  | `App _, _ -> failwith "called_at_tail"
  | `IfElse (_, t, e), [] -> called_at_tail n f' t || called_at_tail n f' e
  | `Lam (i, e), [_] -> (not (Var.equal i f')) && called_at_tail n f' e
  | `Case (`Product fs), [_] ->
    let v = `Var (Var.fresh Loc.dummy) in
    fs |> List.exists (fun (_, f) -> called_at_tail n f' (`App (f, v)))
  | _ -> false

let always_selected i' =
  let rec always_selected = function
    | `Const _ -> true
    | `Var i -> not (Var.equal i i')
    | `Lam (i, e) -> Var.equal i' i || always_selected e
    | `App (f, x) -> always_selected f && always_selected x
    | `IfElse (c, t, e) ->
      always_selected c && always_selected t && always_selected e
    | `Product fs -> fs |> List.for_all (snd >>> always_selected)
    | `Mu e | `Inject (_, e) | `Case e -> always_selected e
    | `Select (`Var i, l) when Var.equal i i' -> always_selected l
    | `Select (e, l) -> always_selected e && always_selected l
  in
  always_selected

let is_immediately_evaluated i' =
  let rec is_immediately_evaluated e =
    match unapp e with
    | `Var i, xs -> Var.equal i i' || [] <> xs
    | `Const _, xs -> List.exists is_immediately_evaluated xs
    | `Lam _, [] -> false
    | `Lam (i, e), x :: xs ->
      is_immediately_evaluated x
      || List.exists is_immediately_evaluated xs
      || ((not (Var.equal i i')) && is_immediately_evaluated (apps e xs))
    | `IfElse (c, t, e), xs ->
      is_immediately_evaluated c
      || List.exists is_immediately_evaluated xs
      ||
      let xs = xs |> List.map (fun _ -> `Var (Var.fresh Loc.dummy)) in
      is_immediately_evaluated (apps t xs)
      || is_immediately_evaluated (apps e xs)
    | `Product fs, _ -> fs |> List.exists (snd >>> is_immediately_evaluated)
    | `Select (e, l), [] ->
      is_immediately_evaluated e || is_immediately_evaluated l
    | `Inject (_, e), _ -> is_immediately_evaluated e
    | `Case cs, [] -> is_immediately_evaluated cs
    | `Case (`Product cs), (_ :: _ as xs) ->
      List.exists is_immediately_evaluated xs
      ||
      let xs = xs |> List.map (fun _ -> `Var (Var.fresh Loc.dummy)) in
      cs |> List.exists (fun (_, f) -> is_immediately_evaluated (apps f xs))
    | _ -> true
  in
  is_immediately_evaluated
