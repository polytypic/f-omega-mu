open FomPP
open FomBasis
open FomSource

module Const = struct
  type t =
    [ `Arrow
    | `Bool
    | `Int
    | `Product of Label.t list
    | `Sum of Label.t list
    | `String ]

  (* Comparison *)

  let equal lhs rhs =
    match (lhs, rhs) with
    | `Arrow, `Arrow | `Bool, `Bool | `Int, `Int | `String, `String -> true
    | `Product lhs, `Product rhs | `Sum lhs, `Sum rhs ->
      ListExt.equal_with Label.equal lhs rhs
    | _ -> false

  let index = function
    | `Arrow -> 0
    | `Bool -> 1
    | `Int -> 2
    | `Product _ -> 3
    | `Sum _ -> 4
    | `String -> 5

  let compare lhs rhs =
    match (lhs, rhs) with
    | `Arrow, `Arrow -> 0
    | `Bool, `Bool -> 0
    | `Int, `Int -> 0
    | `Product lhs, `Product rhs | `Sum lhs, `Sum rhs ->
      ListExt.compare_with Label.compare lhs rhs
    | `String, `String -> 0
    | _ -> index lhs - index rhs

  (* Kinding *)

  let kind_of at t =
    let star = `Star at in
    match t with
    | `Arrow -> `Arrow (at, star, `Arrow (at, star, star))
    | `Bool | `Int | `String -> star
    | `Product labels | `Sum labels ->
      List.fold_left (fun result _ -> `Arrow (at, star, result)) star labels

  (* Formatting *)

  let pp =
    let labeled labels =
      labels |> List.map Label.pp |> separate (concat [comma; break 1])
    in
    let int = string "int" in
    let bool = string "bool" in
    let string = string "string" in
    function
    | `Arrow -> arrow_right
    | `Bool -> bool
    | `Int -> int
    | `String -> string
    | `Product labels -> labeled labels |> braces
    | `Sum labels -> labeled labels |> angles
end

module Id = Id.Make ()

type t =
  [ `Mu of Loc.t * t
  | `Const of Loc.t * Const.t
  | `Var of Loc.t * Id.t
  | `Lam of Loc.t * Id.t * Kind.t * t
  | `App of Loc.t * t * t
  | `ForAll of Loc.t * t
  | `Exists of Loc.t * t ]

let at = function
  | `Mu (at, _)
  | `Const (at, _)
  | `Var (at, _)
  | `Lam (at, _, _, _)
  | `App (at, _, _)
  | `ForAll (at, _)
  | `Exists (at, _) ->
    at

(* Macros *)

let arrow at dom cod = `App (at, `App (at, `Const (at, `Arrow), dom), cod)
let var_of_label ({it; at} : Label.t) = `Var (at, Id.id at it)

let labeled tag at fields =
  let fields = List.sort (Compare.the fst Label.compare) fields in
  let labels = List.map fst fields in
  let typs = List.map snd fields in
  let ctor = `Const (at, tag labels) in
  List.fold_left (fun f x -> `App (at, f, x)) ctor typs

let product at = labeled (fun x -> `Product x) at
let sum at = labeled (fun x -> `Sum x) at

(* Substitution *)

let rec is_free id = function
  | `Const _ -> false
  | `Var (_, id') -> Id.equal id id'
  | `Lam (_, id', _, body) -> (not (Id.equal id id')) && is_free id body
  | `App (_, fn, arg) -> is_free id fn || is_free id arg
  | `Mu (_, typ) | `ForAll (_, typ) | `Exists (_, typ) -> is_free id typ

let rec subst id the inn =
  match inn with
  | `Mu (at, typ) -> `Mu (at, subst id the typ)
  | `Const _ -> inn
  | `Var (_, id') -> if Id.equal id id' then the else inn
  | `Lam (at, id', kind, body) ->
    if Id.equal id id' then
      inn
    else if is_free id' the then
      let id'' = Id.freshen id' in
      let vid'' = `Var (at, id'') in
      `Lam (at, id'', kind, subst id the (subst id' vid'' body))
    else
      `Lam (at, id', kind, subst id the body)
  | `App (at, fn, arg) -> `App (at, subst id the fn, subst id the arg)
  | `ForAll (at, typ) -> `ForAll (at, subst id the typ)
  | `Exists (at, typ) -> `Exists (at, subst id the typ)

(* Comparison *)

let index = function
  | `Mu _ -> 0
  | `Const _ -> 1
  | `Var _ -> 2
  | `Lam _ -> 3
  | `App _ -> 4
  | `ForAll _ -> 5
  | `Exists _ -> 6

let rec compare lhs rhs =
  match (lhs, rhs) with
  | `Mu (_, lhs), `Mu (_, rhs) -> compare lhs rhs
  | `Const (_, lhs), `Const (_, rhs) -> Const.compare lhs rhs
  | `Var (_, lhs), `Var (_, rhs) -> Id.compare lhs rhs
  | `Lam (_, lhs_id, lhs_kind, lhs_typ), `Lam (_, rhs_id, rhs_kind, rhs_typ) ->
    Id.compare lhs_id rhs_id <>? fun () ->
    Kind.compare lhs_kind rhs_kind <>? fun () -> compare lhs_typ rhs_typ
  | `App (_, lhs_fn, lhs_arg), `App (_, rhs_fn, rhs_arg) ->
    compare lhs_fn rhs_fn <>? fun () -> compare lhs_arg rhs_arg
  | `ForAll (_, lhs), `ForAll (_, rhs) | `Exists (_, lhs), `Exists (_, rhs) ->
    compare lhs rhs
  | _ -> index lhs - index rhs

(* *)

let linearize typ =
  let rec recurse = function
    | `App (_, f, x) ->
      let f, xs = recurse f in
      (f, x :: xs)
    | typ -> (typ, [])
  in
  let f, xs = recurse typ in
  (f, List.rev xs)

let rec arity_and_result = function
  | `App (_, `App (_, `Const (_, `Arrow), _), result) ->
    let n, result = arity_and_result result in
    (n + 1, result)
  | typ -> (0, typ)

(* *)

let is_int = function `Const (_, `Int) -> true | _ -> false
let is_bool = function `Const (_, `Bool) -> true | _ -> false

(* Formatting *)

let rec binding atomize head id kind body =
  let kind_annot =
    match kind with
    | `Star _ -> empty
    | _ -> [colon; break_0; Kind.pp kind] |> concat
  in
  [
    [head; Id.pp id; kind_annot] |> concat |> nest 1 |> group;
    [dot; break_0; pp false body] |> concat |> nest 1 |> group;
  ]
  |> concat |> parens_if atomize

and quantifier atomize symbol (typ : t) =
  match typ with
  | `Lam (_, id, kind, body) -> binding atomize symbol id kind body
  | _ -> [symbol; pp false typ |> parens] |> concat |> nest 1

and labeled labels typs =
  List.combine labels typs
  |> List.map (function
       | l, `Var (_, {Id.it = i; _}) when i = l.Label.it -> Label.pp l
       | label, typ ->
         [Label.pp label; colon; break_1; pp false typ]
         |> concat |> nest 1 |> group)
  |> separate comma_break_1

and pp atomize (typ : t) =
  match typ with
  | `Const (_, const) -> Const.pp const
  | `Var (_, id) -> Id.pp id
  | `Lam (_, id, kind, body) -> binding atomize lambda_lower id kind body
  | `App (_, fn, arg) -> (
    match linearize typ with
    | `Const (_, `Arrow), [dom; cod] ->
      [pp true dom; break_1; arrow_right; space; pp false cod]
      |> concat |> parens_if atomize
    | `Const (_, `Product labels), typs
      when List.length labels = List.length typs ->
      labeled labels typs |> braces
    | `Const (_, `Sum labels), typs when List.length labels = List.length typs
      ->
      labeled labels typs |> brackets
    | _ -> [pp true fn; break_1; pp true arg] |> concat |> group)
  | `Mu (_, typ) -> quantifier atomize FomPP.mu_lower typ
  | `ForAll (_, typ) -> quantifier atomize FomPP.for_all typ
  | `Exists (_, typ) -> quantifier atomize FomPP.exists typ

let pp typ = pp false typ |> group
