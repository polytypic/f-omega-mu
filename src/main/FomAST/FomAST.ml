open FomBasis
open FomPP
open FomSource

let ignore2 _ _ = ()

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

  (* *)

  let arity =
    let rec loop n = function
      | `Star _ -> n
      | `Arrow (_, _, c) -> loop (n + 1) c
    in
    loop 0

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

  (** If both labels are numeric, comparison is done by numeric value. *)
  let compare lhs rhs =
    let lhs = to_string lhs in
    let rhs = to_string rhs in
    try int_of_string lhs - int_of_string rhs
    with Failure _ -> String.compare lhs rhs
end

module Tuple = struct
  let is_tuple labels =
    labels
    |> ListExt.for_alli (fun i (l, _) ->
           Label.to_string l = Int.to_string (i + 1))
end

module Typ = struct
  module Const = struct
    type t = [`Bool | `Int | `String]

    (* Comparison *)

    let equal lhs rhs =
      match (lhs, rhs) with
      | `Bool, `Bool | `Int, `Int | `String, `String -> true
      | _ -> false

    let index = function `Bool -> 0 | `Int -> 1 | `String -> 2

    let compare lhs rhs =
      match (lhs, rhs) with
      | `Bool, `Bool | `Int, `Int | `String, `String -> 0
      | _ -> index lhs - index rhs

    (* Kinding *)

    let kind_of at t =
      let star = `Star at in
      match t with `Bool | `Int | `String -> star

    (* Formatting *)

    let pp = function `Bool -> bool' | `Int -> int' | `String -> string'
  end

  module Id = Id.Make ()

  type 't f =
    [ `Mu of Loc.t * 't
    | `Const of Loc.t * Const.t
    | `Var of Loc.t * Id.t
    | `Lam of Loc.t * Id.t * Kind.t * 't
    | `App of Loc.t * 't * 't
    | `ForAll of Loc.t * 't
    | `Exists of Loc.t * 't
    | `Arrow of Loc.t * 't * 't
    | `Product of Loc.t * (Label.t * 't) list
    | `Sum of Loc.t * (Label.t * 't) list ]

  type t = [ | t f]

  let at = function
    | `Mu (at, _)
    | `Const (at, _)
    | `Var (at, _)
    | `Lam (at, _, _, _)
    | `App (at, _, _)
    | `ForAll (at, _)
    | `Exists (at, _)
    | `Arrow (at, _, _)
    | `Product (at, _)
    | `Sum (at, _) ->
      at

  let set_at at = function
    | `Mu (_, t) -> `Mu (at, t)
    | `Const (_, c) -> `Const (at, c)
    | `Var (_, i) -> `Var (at, i)
    | `Lam (_, i, k, t) -> `Lam (at, i, k, t)
    | `App (_, f, x) -> `App (at, f, x)
    | `ForAll (_, t) -> `ForAll (at, t)
    | `Exists (_, t) -> `Exists (at, t)
    | `Arrow (_, d, c) -> `Arrow (at, d, c)
    | `Product (_, ls) -> `Product (at, ls)
    | `Sum (_, ls) -> `Sum (at, ls)

  (* Macros *)

  let sort labels = List.sort (Compare.the fst Label.compare) labels
  let product at fs = `Product (at, sort fs)
  let sum at cs = `Sum (at, sort cs)
  let zero at = `Sum (at, [])

  (* Type predicates *)

  let is_int = function `Const (_, `Int) -> true | _ -> false

  (* Type applications *)

  let app at = List.fold_left (fun f x -> `App (at, f, x))

  let unapp t =
    let rec loop xs = function
      | `App (_, f, x) -> loop (x :: xs) f
      | f -> (f, xs)
    in
    loop [] t

  let arity_and_result t =
    let rec loop n = function
      | `Arrow (_, _, c) -> loop (n + 1) c
      | r -> (n, r)
    in
    loop 0 t

  (* Substitution *)

  module IdSet = Set.Make (Id)

  let rec free = function
    | `Const _ -> IdSet.empty
    | `Var (_, i) -> IdSet.singleton i
    | `Lam (_, i, _, e) -> free e |> IdSet.remove i
    | `App (_, f, x) -> IdSet.union (free f) (free x)
    | `Mu (_, e) | `ForAll (_, e) | `Exists (_, e) -> free e
    | `Arrow (_, d, c) -> IdSet.union (free d) (free c)
    | `Product (_, ls) | `Sum (_, ls) ->
      ls |> List.fold_left (fun s (_, t) -> IdSet.union s (free t)) IdSet.empty

  module Env = Map.Make (Id)

  let rec subst_rec replaced env = function
    | `Mu (at, t) as inn ->
      let t' = subst_rec replaced env t in
      if t == t' then inn else `Mu (at, t')
    | `Const _ as inn -> inn
    | `Var (_, i) as inn -> (
      match Env.find_opt i env with
      | None -> inn
      | Some t ->
        replaced i t;
        subst_rec replaced env t)
    | `Lam (at, i, k, t) as inn ->
      let env = Env.remove i env in
      if Env.is_empty env then
        inn
      else
        let t' = subst_rec replaced env t in
        if t == t' then inn else `Lam (at, i, k, t')
    | `App (at, f, x) as inn ->
      let f' = subst_rec replaced env f in
      let x' = subst_rec replaced env x in
      if f == f' && x == x' then inn else `App (at, f', x')
    | `ForAll (at, t) as inn ->
      let t' = subst_rec replaced env t in
      if t == t' then inn else `ForAll (at, t')
    | `Exists (at, t) as inn ->
      let t' = subst_rec replaced env t in
      if t == t' then inn else `Exists (at, t')
    | `Arrow (at, d, c) as inn ->
      let d' = subst_rec replaced env d in
      let c' = subst_rec replaced env c in
      if d == d' && c == c' then inn else `Arrow (at, d', c')
    | `Product (at, ls) as inn ->
      let ls' =
        ls |> ListExt.map_phys_eq (Pair.map_phys_eq id (subst_rec replaced env))
      in
      if ls == ls' then inn else `Product (at, ls')
    | `Sum (at, ls) as inn ->
      let ls' =
        ls |> ListExt.map_phys_eq (Pair.map_phys_eq id (subst_rec replaced env))
      in
      if ls == ls' then inn else `Sum (at, ls')

  let subst_rec ?(replaced = ignore2) env t =
    if Env.is_empty env then t else subst_rec replaced env t

  let rec is_free id = function
    | `Const _ -> false
    | `Var (_, id') -> Id.equal id id'
    | `Lam (_, id', _, body) -> (not (Id.equal id id')) && is_free id body
    | `App (_, fn, arg) -> is_free id fn || is_free id arg
    | `Mu (_, typ) | `ForAll (_, typ) | `Exists (_, typ) -> is_free id typ
    | `Arrow (_, d, c) -> is_free id d || is_free id c
    | `Product (_, ls) | `Sum (_, ls) ->
      ls |> List.exists (fun (_, t) -> is_free id t)

  let rec subst_par replaced env = function
    | `Mu (at, t) as inn ->
      let t' = subst_par replaced env t in
      if t == t' then inn else `Mu (at, t')
    | `Const _ as inn -> inn
    | `Var (_, i) as inn -> (
      match Env.find_opt i env with
      | None -> inn
      | Some t ->
        replaced i t;
        t)
    | `Lam (at, i, k, t) as inn ->
      let env = Env.remove i env in
      if Env.is_empty env then
        inn
      else if Env.exists (fun i' t' -> is_free i t' && is_free i' t) env then
        let i' = Id.freshen i in
        let v' = `Var (at, i') in
        let t' = subst_par replaced (Env.add i v' env) t in
        if t == t' then inn else `Lam (at, i', k, t')
      else
        let t' = subst_par replaced env t in
        if t == t' then inn else `Lam (at, i, k, t')
    | `App (at, f, x) as inn ->
      let f' = subst_par replaced env f in
      let x' = subst_par replaced env x in
      if f == f' && x == x' then inn else `App (at, f', x')
    | `ForAll (at, t) as inn ->
      let t' = subst_par replaced env t in
      if t == t' then inn else `ForAll (at, t')
    | `Exists (at, t) as inn ->
      let t' = subst_par replaced env t in
      if t == t' then inn else `Exists (at, t')
    | `Arrow (at, d, c) as inn ->
      let d' = subst_par replaced env d in
      let c' = subst_par replaced env c in
      if d == d' && c == c' then inn else `Arrow (at, d', c')
    | `Product (at, ls) as inn ->
      let ls' =
        ls |> ListExt.map_phys_eq (Pair.map_phys_eq id (subst_par replaced env))
      in
      if ls == ls' then inn else `Product (at, ls')
    | `Sum (at, ls) as inn ->
      let ls' =
        ls |> ListExt.map_phys_eq (Pair.map_phys_eq id (subst_par replaced env))
      in
      if ls == ls' then inn else `Sum (at, ls')

  let subst ?(replaced = ignore2) i' t' t =
    subst_par replaced (Env.add i' t' Env.empty) t

  let subst_par ?(replaced = ignore2) env t =
    if Env.is_empty env then t else subst_par replaced env t

  let rec norm = function
    | `Mu (at, t) as inn -> (
      match norm t with
      | `Lam (_, i, _, t) when not (is_free i t) -> t
      | t' -> if t == t' then inn else `Mu (at, t'))
    | `Const _ as inn -> inn
    | `Var _ as inn -> inn
    | `Lam (at, i, k, t) as inn -> (
      match norm t with
      | `App (_, f, `Var (_, i')) when Id.equal i i' && not (is_free i f) -> f
      | t' -> if t == t' then inn else `Lam (at, i, k, t'))
    | `App (at, f, x) as inn -> (
      let x' = norm x in
      match norm f with
      | `Lam (_, i, _, t) -> norm (subst i x' t)
      | f' -> if f == f' && x == x' then inn else `App (at, f', x'))
    | `ForAll (at, t) as inn ->
      let t' = norm t in
      if t == t' then inn else `ForAll (at, t')
    | `Exists (at, t) as inn ->
      let t' = norm t in
      if t == t' then inn else `Exists (at, t')
    | `Arrow (at, d, c) as inn ->
      let d' = norm d in
      let c' = norm c in
      if d == d' && c == c' then inn else `Arrow (at, d', c')
    | `Product (at, ls) as inn ->
      let ls' = ls |> ListExt.map_phys_eq (Pair.map_phys_eq id norm) in
      if ls == ls' then inn else `Product (at, ls')
    | `Sum (at, ls) as inn ->
      let ls' = ls |> ListExt.map_phys_eq (Pair.map_phys_eq id norm) in
      if ls == ls' then inn else `Sum (at, ls')

  (* Comparison *)

  let index = function
    | `Mu _ -> 0
    | `Const _ -> 1
    | `Var _ -> 2
    | `Lam _ -> 3
    | `App _ -> 4
    | `ForAll _ -> 5
    | `Exists _ -> 6
    | `Arrow _ -> 7
    | `Product _ -> 8
    | `Sum _ -> 9

  let rec compare lhs rhs =
    if lhs == rhs then
      0
    else
      match (lhs, rhs) with
      | `Mu (_, lhs), `Mu (_, rhs) -> compare lhs rhs
      | `Const (_, lhs), `Const (_, rhs) -> Const.compare lhs rhs
      | `Var (_, lhs), `Var (_, rhs) -> Id.compare lhs rhs
      | `Lam (_, lhs_id, lhs_kind, lhs_typ), `Lam (_, rhs_id, rhs_kind, rhs_typ)
        ->
        Kind.compare lhs_kind rhs_kind <>? fun () ->
        if Id.equal lhs_id rhs_id then
          compare lhs_typ rhs_typ
        else
          let v = `Var (Loc.dummy, Id.fresh Loc.dummy) in
          compare (subst lhs_id v lhs_typ) (subst rhs_id v rhs_typ)
      | `App (_, lhs_fn, lhs_arg), `App (_, rhs_fn, rhs_arg) ->
        compare lhs_fn rhs_fn <>? fun () -> compare lhs_arg rhs_arg
      | `ForAll (_, lhs), `ForAll (_, rhs) | `Exists (_, lhs), `Exists (_, rhs)
        ->
        compare lhs rhs
      | `Arrow (_, lhs_d, lhs_c), `Arrow (_, rhs_d, rhs_c) ->
        compare lhs_d rhs_d <>? fun () -> compare lhs_c rhs_c
      | `Product (_, lhs_ls), `Product (_, rhs_ls)
      | `Sum (_, lhs_ls), `Sum (_, rhs_ls) ->
        ListExt.compare_with
          (fun (lhs_l, lhs_t) (rhs_l, rhs_t) ->
            Label.compare lhs_l rhs_l <>? fun () -> compare lhs_t rhs_t)
          lhs_ls rhs_ls
      | _ -> index lhs - index rhs

  (* Formatting *)

  let prec_min = 0
  let prec_arrow = 1
  let prec_app = 2

  (* *)

  let some_spaces = Some spaces

  let rec hanging = function
    | `Lam _ | `Mu (_, `Lam _) | `ForAll (_, `Lam _) | `Exists (_, `Lam _) ->
      some_spaces
    | `Product _ | `Sum _ -> some_spaces
    | `App _ as t -> (
      match unapp t with `Var _, [x] -> hanging x | _ -> None)
    | _ -> None

  let rec binding prec_outer head i k t =
    [
      [head; Id.pp i; Kind.pp_annot k; dot] |> concat |> nest 2 |> group;
      (match hanging t with
      | Some _ -> pp prec_min t
      | None -> [break_0; pp prec_min t |> group] |> concat |> nest 2 |> group);
    ]
    |> concat
    |> if prec_min < prec_outer then egyptian parens 2 else id

  and quantifier prec_outer symbol (typ : t) =
    match typ with
    | `Lam (_, id, kind, body) -> binding prec_outer symbol id kind body
    | _ -> [symbol; pp prec_min typ |> egyptian parens 2] |> concat

  and labeled labels =
    labels
    |> List.stable_sort (Compare.the (fst >> Label.at >> fst) Pos.compare)
    |> List.map (function
         | l, `Var (_, i) when Id.name i = Label.name l -> Label.pp l
         | label, typ ->
           [
             [Label.pp label; colon] |> concat;
             (match hanging typ with
             | Some (lhs, _) -> [lhs; pp prec_min typ] |> concat
             | None -> [break_1; pp prec_min typ] |> concat |> nest 2 |> group);
           ]
           |> concat)
    |> separate comma_break_1

  and tuple labels =
    labels |> List.map (snd >> pp prec_min) |> separate comma_break_1

  and pp prec_outer (typ : t) =
    match typ with
    | `Const (_, const) -> Const.pp const
    | `Var (_, id) -> Id.pp id
    | `Lam (_, id, kind, body) -> binding prec_outer lambda_lower id kind body
    | `Mu (_, typ) -> quantifier prec_outer FomPP.mu_lower typ
    | `ForAll (_, typ) -> quantifier prec_outer FomPP.for_all typ
    | `Exists (_, typ) -> quantifier prec_outer FomPP.exists typ
    | `Arrow (_, dom, cod) ->
      [
        pp (prec_arrow + 1) dom;
        [
          (match hanging cod with
          | Some (lhs, _) -> [space_arrow_right; lhs] |> concat
          | None -> space_arrow_right_break_1);
          pp (prec_arrow - 1) cod;
        ]
        |> concat;
      ]
      |> concat
      |> if prec_arrow < prec_outer then egyptian parens 2 else id
    | `Product (_, labels) ->
      if Tuple.is_tuple labels then
        tuple labels |> egyptian parens 2
      else
        labeled labels |> egyptian braces 2
    | `Sum (_, labels) -> labeled labels |> egyptian brackets 2
    | `App (_, _, _) -> (
      match unapp typ with
      | f, xs ->
        pp prec_app f :: (xs |> List.map (pp (prec_app + 1) >> group))
        |> separate break_1
        |> if prec_app < prec_outer then egyptian parens 2 else group)

  let pp typ = pp prec_min typ |> group
end

module Exp = struct
  let bool = `Const (Loc.dummy, `Bool)
  let int = `Const (Loc.dummy, `Int)

  module Const = struct
    type ('nat, 't) t =
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
      | `OpEq of 't
      | `OpEqNot of 't
      | `OpLogicalAnd
      | `OpLogicalNot
      | `OpLogicalOr ]

    (* Typing *)

    let type_of at = function
      | `LitBool _ -> `Const (at, `Bool)
      | `LitNat _ -> `Const (at, `Int)
      | `LitString _ -> `Const (at, `String)
      | `OpArithAdd | `OpArithSub | `OpArithMul | `OpArithDiv | `OpArithRem ->
        `Arrow (at, int, `Arrow (at, int, int))
      | `OpArithPlus | `OpArithMinus -> `Arrow (at, int, int)
      | `OpCmpLt | `OpCmpLtEq | `OpCmpGt | `OpCmpGtEq ->
        `Arrow (at, int, `Arrow (at, int, bool))
      | `OpEq typ | `OpEqNot typ -> `Arrow (at, typ, `Arrow (at, typ, bool))
      | `OpLogicalAnd | `OpLogicalOr ->
        `Arrow (at, bool, `Arrow (at, bool, bool))
      | `OpLogicalNot -> `Arrow (at, bool, bool)

    (* Substitution *)

    let map_typ tu = function
      | ( `LitBool _ | `LitNat _ | `LitString _ | `OpArithAdd | `OpArithDiv
        | `OpArithMinus | `OpArithMul | `OpArithPlus | `OpArithRem | `OpArithSub
        | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpLogicalAnd
        | `OpLogicalNot | `OpLogicalOr ) as c ->
        c
      | `OpEq t -> `OpEq (tu t)
      | `OpEqNot t -> `OpEqNot (tu t)

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
  module Env = Map.Make (Id)

  type ('e, 't) f =
    [ `Const of Loc.t * (Bigint.t, 't) Const.t
    | `Var of Loc.t * Id.t
    | `Lam of Loc.t * Id.t * 't * 'e
    | `App of Loc.t * 'e * 'e
    | `Gen of Loc.t * Typ.Id.t * Kind.t * 'e
    | `Inst of Loc.t * 'e * 't
    | `LetIn of Loc.t * Id.t * 'e * 'e
    | `Mu of Loc.t * 'e
    | `IfElse of Loc.t * 'e * 'e * 'e
    | `Product of Loc.t * (Label.t * 'e) list
    | `Select of Loc.t * 'e * Label.t
    | `Inject of Loc.t * Label.t * 'e
    | `Case of Loc.t * 'e
    | `Pack of Loc.t * 't * 'e * 't
    | `UnpackIn of Loc.t * Typ.Id.t * Id.t * 'e * 'e
    | `Target of Loc.t * 't * string ]

  type t = [ | (t, Typ.t) f]

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
    | `Inject (at, _, _)
    | `Case (at, _)
    | `Pack (at, _, _, _)
    | `UnpackIn (at, _, _, _, _)
    | `Target (at, _, _) ->
      at
end
