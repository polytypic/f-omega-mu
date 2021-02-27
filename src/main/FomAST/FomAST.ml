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
      [pp true dom; break_1; arrow_right; space; pp false cod]
      |> concat |> parens_if atomize

  let pp kind = pp false kind |> FomPP.group
end

module Label = Id.Make ()

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
end

module Exp = struct
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

  (* Substitution *)

  let rec is_free i it =
    match it with
    | `Const _ | `Target _ -> false
    | `Var (_, i') -> Id.equal i' i
    | `Lam (_, i', _, e) -> (not (Id.equal i' i)) && is_free i e
    | `App (_, fn, arg) -> is_free i fn || is_free i arg
    | `Gen (_, _, _, e) -> is_free i e
    | `Inst (_, e, _) -> is_free i e
    | `LetIn (_, i', v, e) ->
      is_free i v || ((not (Id.equal i' i)) && is_free i e)
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
end
