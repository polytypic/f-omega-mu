open FomBasis
open FomPP
open FomSource

module Kind = struct
  type t = [`Star of Loc.t | `Arrow of Loc.t * t * t]

  let at = function `Star at -> at | `Arrow (at, _, _) -> at

  (* Comparison *)

  let index = function `Star _ -> 0 | `Arrow _ -> 1

  let rec compare lhs rhs =
    match (lhs, rhs) with
    | `Star _, `Star _ -> 0
    | `Arrow (_, lhs_dom, lhs_cod), `Arrow (_, rhs_dom, rhs_cod) ->
      compare lhs_dom rhs_dom <>? fun () -> compare lhs_cod rhs_cod
    | _ -> index lhs - index rhs

  (* Formatting *)

  let rec pp atomize kind =
    let open FomPP in
    match kind with
    | `Star _ -> star
    | `Arrow (_, dom, cod) ->
      [pp true dom; space_arrow_right_break_1; pp false cod]
      |> concat
      |> if atomize then egyptian parens 2 else id

  let pp kind = pp false kind |> FomPP.group

  let pp_annot = function
    | `Star _ -> empty
    | kind -> [colon; pp kind |> align] |> concat
end

module Label = struct
  include Id.Make ()

  (** If both labels are numberic, comparison is done by numeric value. *)
  let compare {it = lhs; _} {it = rhs; _} =
    try int_of_string lhs - int_of_string rhs
    with Failure _ -> String.compare lhs rhs
end

module Tuple = struct
  let is_tuple =
    ListExt.for_alli (fun i l -> l.Label.it = Int.to_string (i + 1))
end

module Typ = struct
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
      let labeled brackets labels =
        labels |> List.map Label.pp |> separate comma_break_1
        |> egyptian brackets 2
      in
      function
      | `Arrow -> arrow_right
      | `Bool -> bool'
      | `Int -> int'
      | `String -> string'
      | `Product labels -> labeled braces labels
      | `Sum labels -> labeled brackets labels
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

  let labeled tag at fields =
    let fields = List.sort (Compare.the fst Label.compare) fields in
    let labels = List.map fst fields in
    let typs = List.map snd fields in
    let ctor = `Const (at, tag labels) in
    List.fold_left (fun f x -> `App (at, f, x)) ctor typs

  let product at = labeled (fun x -> `Product x) at
  let sum at = labeled (fun x -> `Sum x) at
  let zero at = `Const (at, `Sum [])

  (* Substitution *)

  let rec is_free id = function
    | `Const _ -> false
    | `Var (_, id') -> Id.equal id id'
    | `Lam (_, id', _, body) -> (not (Id.equal id id')) && is_free id body
    | `App (_, fn, arg) -> is_free id fn || is_free id arg
    | `Mu (_, typ) | `ForAll (_, typ) | `Exists (_, typ) -> is_free id typ

  let rec subst ?(replaced : Id.t -> unit = ignore) i' t' = function
    | `Mu (at, t) -> `Mu (at, subst ~replaced i' t' t)
    | `Const _ as inn -> inn
    | `Var (_, i) as inn ->
      if Id.equal i i' then (
        replaced i;
        t')
      else
        inn
    | `Lam (at, i, k, t) as inn ->
      if Id.equal i i' then
        inn
      else if is_free i t' then
        let i'' = Id.freshen i in
        let vi'' = `Var (at, i'') in
        `Lam (at, i'', k, subst ~replaced i' t' (subst i vi'' t))
      else
        `Lam (at, i, k, subst ~replaced i' t' t)
    | `App (at, f, x) ->
      `App (at, subst ~replaced i' t' f, subst ~replaced i' t' x)
    | `ForAll (at, t) -> `ForAll (at, subst ~replaced i' t' t)
    | `Exists (at, t) -> `Exists (at, subst ~replaced i' t' t)

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
    | `Lam (_, lhs_id, lhs_kind, lhs_typ), `Lam (_, rhs_id, rhs_kind, rhs_typ)
      ->
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

  let some_spaces = Some spaces

  let rec hanging = function
    | `Lam _ | `Mu (_, `Lam _) | `ForAll (_, `Lam _) | `Exists (_, `Lam _) ->
      some_spaces
    | `App _ as t -> (
      match linearize t with
      | (`Const (_, `Product labels), typs | `Const (_, `Sum labels), typs)
        when List.length labels = List.length typs ->
        some_spaces
      | `Var _, [x] -> hanging x
      | _ -> None)
    | _ -> None

  let rec binding atomize head i k t =
    [
      [head; Id.pp i; Kind.pp_annot k; dot] |> concat |> nest 2 |> group;
      (match hanging t with
      | Some _ -> pp false t
      | None -> [break_0; pp false t |> group] |> concat |> nest 2 |> group);
    ]
    |> concat
    |> if atomize then egyptian parens 2 else id

  and quantifier atomize symbol (typ : t) =
    match typ with
    | `Lam (_, id, kind, body) -> binding atomize symbol id kind body
    | _ -> [symbol; pp false typ |> egyptian parens 2] |> concat

  and labeled labels typs =
    List.combine labels typs
    |> List.stable_sort
         (Compare.the (fun ({Label.at; _}, _) -> fst at) Pos.compare)
    |> List.map (function
         | l, `Var (_, {Id.it = i; _}) when i = l.Label.it -> Label.pp l
         | label, typ ->
           [
             [Label.pp label; colon] |> concat;
             (match hanging typ with
             | Some (lhs, _) -> [lhs; pp false typ] |> concat
             | None -> [break_1; pp false typ] |> concat |> nest 2 |> group);
           ]
           |> concat)
    |> separate comma_break_1

  and tuple typs = typs |> List.map (pp false) |> separate comma_break_1

  and pp atomize (typ : t) =
    match typ with
    | `Const (_, const) -> Const.pp const
    | `Var (_, id) -> Id.pp id
    | `Lam (_, id, kind, body) -> binding atomize lambda_lower id kind body
    | `App (_, fn, arg) -> (
      match linearize typ with
      | `Const (_, `Arrow), [dom; cod] ->
        [
          pp true dom;
          [
            (match hanging cod with
            | Some (lhs, _) -> [space_arrow_right; lhs] |> concat
            | None -> space_arrow_right_break_1);
            pp false cod;
          ]
          |> concat;
        ]
        |> concat
        |> if atomize then egyptian parens 2 else id
      | `Const (_, `Product labels), typs
        when List.length labels = List.length typs ->
        if Tuple.is_tuple labels then
          tuple typs |> egyptian parens 2
        else
          labeled labels typs |> egyptian braces 2
      | `Const (_, `Sum labels), typs when List.length labels = List.length typs
        ->
        labeled labels typs |> egyptian brackets 2
      | _ ->
        [
          pp true fn;
          (match hanging arg with Some (sep, _) -> sep | None -> break_1);
          pp true arg;
        ]
        |> concat |> group)
    | `Mu (_, typ) -> quantifier atomize FomPP.mu_lower typ
    | `ForAll (_, typ) -> quantifier atomize FomPP.for_all typ
    | `Exists (_, typ) -> quantifier atomize FomPP.exists typ

  let pp typ = pp false typ |> group
end

module Exp = struct
  let bool = `Const (Loc.dummy, `Bool)
  let int = `Const (Loc.dummy, `Int)

  module Const = struct
    type 'nat t =
      [ `LitBool of bool
      | `LitNat of 'nat
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
      | `OpLogicalAnd | `OpLogicalOr ->
        Typ.arrow at bool (Typ.arrow at bool bool)
      | `OpLogicalNot -> Typ.arrow at bool bool

    (* Substitution *)

    let subst i t c =
      match c with
      | `LitBool _ | `LitNat _ | `LitString _ | `OpArithAdd | `OpArithDiv
      | `OpArithMinus | `OpArithMul | `OpArithPlus | `OpArithRem | `OpArithSub
      | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpLogicalAnd
      | `OpLogicalNot | `OpLogicalOr ->
        c
      | `OpEq t' -> `OpEq (Typ.subst i t t')
      | `OpEqNot t' -> `OpEqNot (Typ.subst i t t')

    let lit_false = `LitBool false
    let lit_true = `LitBool true

    (* Formatting *)

    let pp = function
      | `LitBool bool -> if bool then true' else false'
      | `LitNat i -> Bigint.to_string i |> utf8string
      | `LitString s -> utf8string s
      | `OpArithAdd -> plus
      | `OpArithDiv -> slash
      | `OpArithMinus -> minus
      | `OpArithMul -> star
      | `OpArithPlus -> plus
      | `OpArithRem -> percent
      | `OpArithSub -> minus
      | `OpCmpGt -> langle
      | `OpCmpGtEq -> greater_equal
      | `OpCmpLt -> rangle
      | `OpCmpLtEq -> less_equal
      | `OpEq t -> [equals; Typ.pp t |> egyptian brackets 2] |> concat
      | `OpEqNot t -> [not_equal; Typ.pp t |> egyptian brackets 2] |> concat
      | `OpLogicalAnd -> logical_and
      | `OpLogicalNot -> logical_not
      | `OpLogicalOr -> logical_or
  end

  module Id = Id.Make ()

  type 't f =
    [ `Const of Loc.t * Bigint.t Const.t
    | `Var of Loc.t * Id.t
    | `Lam of Loc.t * Id.t * Typ.t * 't
    | `App of Loc.t * 't * 't
    | `Gen of Loc.t * Typ.Id.t * Kind.t * 't
    | `Inst of Loc.t * 't * Typ.t
    | `LetIn of Loc.t * Id.t * 't * 't
    | `Mu of Loc.t * 't
    | `IfElse of Loc.t * 't * 't * 't
    | `Product of Loc.t * (Label.t * 't) list
    | `Select of Loc.t * 't * Label.t
    | `Inject of Loc.t * Label.t * 't * Typ.t
    | `Case of Loc.t * 't * 't
    | `Pack of Loc.t * Typ.t * 't * Typ.t
    | `UnpackIn of Loc.t * Typ.Id.t * Id.t * 't * 't
    | `Target of Loc.t * Typ.t * string ]

  type t = [ | t f]

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
end
