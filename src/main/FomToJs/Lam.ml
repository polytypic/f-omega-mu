open FomBasis
open FomSource

(* *)

include LamCore

module Env = struct
  include VarMap

  type nonrec t = LamCore.t t

  let field r = r#env
  let find_opt i = get_as field (find_opt i)
  let adding i e = mapping field (add i e)

  class con =
    object
      val env : t = empty
      method env = Field.make env (fun v -> {<env = v>})
    end
end

module Limit = struct
  type t = int

  let field r = r#limit

  class con =
    object
      val limit : t = 0
      method limit = Field.make limit (fun v -> {<limit = v>})
    end
end

module Seen = struct
  include Set.Make (LamCore)

  let field r = r#seen
  let adding e = mapping field (add e)

  class con =
    object
      val seen : t = empty
      method seen = Field.make seen (fun v -> {<seen = v>})
    end
end

module Renumbering = struct
  include Map.Make (Id.Name)

  type nonrec t = int t

  let field r = r#renumbering

  class con =
    object
      val renumbering : t = empty
      method renumbering = Field.make renumbering (fun v -> {<renumbering = v>})
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

  let is_pure = function
    | `Keep _ -> false
    | `LitBool _ | `LitNat _ | `LitString _ | `OpArithAdd | `OpArithDiv
    | `OpArithMinus | `OpArithMul | `OpArithPlus | `OpArithRem | `OpArithSub
    | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpEq _ | `OpEqNot _
    | `OpLogicalAnd | `OpLogicalNot | `OpLogicalOr | `OpStringCat | `Target _ ->
      true
end

let dummy_var = `Var (Var.fresh Loc.dummy)

let rec is_pure e =
  let* seen = get Seen.field in
  if Seen.mem e seen then return true
  else
    (match unapp e with
    | `App _, _ -> failwith "is_pure"
    (* *)
    | `Case (`Product fs), x :: xs ->
      is_pure x
      &&& List.for_all_fr (fun (_, f) -> is_pure (apps f (dummy_var :: xs))) fs
    | `Case e, [] -> is_pure e
    | `Case _, _ -> return false
    (* *)
    | `Const c, xs ->
      return (Const.is_pure c || List.length xs < Const.arity c)
      &&& List.for_all_fr is_pure xs
    (* *)
    | `IfElse (c, t, e), xs ->
      is_pure c &&& is_pure (apps t xs) &&& is_pure (apps e xs)
    (* *)
    | `Inject (_, e), _ -> is_pure e
    (* *)
    | `Lam _, [] -> return true
    | `Lam (_, e), x :: xs -> is_pure x &&& is_pure (apps e xs)
    (* *)
    | `Mu (`Lam (_, e)), xs -> is_pure (apps e xs)
    | `Mu _, _ -> return false
    (* *)
    | `Product fs, [] -> List.for_all_fr (snd >>> is_pure) fs
    | `Product _, _ :: _ -> return false
    (* *)
    | `Select (e, l), [] -> is_pure e &&& is_pure l
    | `Select _, _ :: _ -> return false
    (* *)
    | `Var _, [] -> return true
    | `Var f, (_ :: _ as xs) -> (
      let* f_opt = Env.find_opt f in
      match f_opt with None -> return false | Some f -> is_pure (apps f xs)))
    |> Seen.adding e

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

let rec always_selected i' = function
  | `Const _ -> true
  | `Var i -> not (Var.equal i i')
  | `Lam (i, e) -> (not (Var.equal i' i)) || always_selected i' e
  | `App (f, x) -> always_selected i' f && always_selected i' x
  | `IfElse (c, t, e) ->
    always_selected i' c || always_selected i' t || always_selected i' e
  | `Product fs -> fs |> List.for_all (snd >>> always_selected i')
  | `Mu e | `Inject (_, e) | `Case e -> always_selected i' e
  | `Select (`Var i, l) when Var.equal i i' -> always_selected i' l
  | `Select (e, l) -> always_selected i' e || always_selected i' l

let rec is_immediately_evaluated i' e =
  match unapp e with
  | `Var i, xs -> Var.equal i i' || [] <> xs
  | `Const _, xs -> List.exists (is_immediately_evaluated i') xs
  | `Lam _, [] -> false
  | `Lam (i, e), x :: xs ->
    is_immediately_evaluated i' x
    || List.exists (is_immediately_evaluated i') xs
    || ((not (Var.equal i i')) && is_immediately_evaluated i' (apps e xs))
  | `IfElse (c, t, e), xs ->
    is_immediately_evaluated i' c
    || List.exists (is_immediately_evaluated i') xs
    ||
    let xs = xs |> List.map (fun _ -> `Var (Var.fresh Loc.dummy)) in
    is_immediately_evaluated i' (apps t xs)
    || is_immediately_evaluated i' (apps e xs)
  | `Product fs, _ -> fs |> List.exists (snd >>> is_immediately_evaluated i')
  | `Select (e, l), [] ->
    is_immediately_evaluated i' e || is_immediately_evaluated i' l
  | `Inject (_, e), _ -> is_immediately_evaluated i' e
  | `Case cs, [] -> is_immediately_evaluated i' cs
  | `Case (`Product cs), (_ :: _ as xs) ->
    List.exists (is_immediately_evaluated i') xs
    ||
    let xs = xs |> List.map (fun _ -> `Var (Var.fresh Loc.dummy)) in
    cs |> List.exists (fun (_, f) -> is_immediately_evaluated i' (apps f xs))
  | _ -> true
