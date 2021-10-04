open FomBasis
open FomPP
open FomSource

module Kind = struct
  module Unk = Id.Make ()
  module UnkMap = Map.Make (Unk)

  type 'k f =
    [`Star of Loc.t | `Arrow of Loc.t * 'k * 'k | `Unk of Loc.t * Unk.t]

  type t = t f

  let at = function `Star at -> at | `Arrow (at, _, _) | `Unk (at, _) -> at

  (* *)

  let fresh at = `Unk (at, Unk.fresh at)

  (* Comparison *)

  let index = function `Star _ -> 0 | `Arrow _ -> 1 | `Unk _ -> 2

  let rec compare lhs rhs =
    if lhs == rhs then
      0
    else
      match (lhs, rhs) with
      | `Star _, `Star _ -> 0
      | `Arrow (_, lhs_dom, lhs_cod), `Arrow (_, rhs_dom, rhs_cod) ->
        compare lhs_dom rhs_dom <>? fun () -> compare lhs_cod rhs_cod
      | `Unk (_, l), `Unk (_, r) -> Unk.compare l r
      | _ -> index lhs - index rhs

  (* *)

  let rec min_arity n = function
    | `Star _ -> n
    | `Arrow (_, _, c) -> min_arity (n + 1) c
    | `Unk _ -> 1

  let min_arity k = min_arity 0 k

  (* *)

  let eq l r =
    match (l, r) with
    | `Star l, `Star r -> l == r
    | `Arrow l, `Arrow r -> eq'3 l r
    | `Unk l, `Unk r -> eq'2 l r
    | _ -> false

  (* *)

  let keep_phys_eq' k k' = if k == k' || eq k k' then k else k'
  let keep_phys_eq fn k = keep_phys_eq' k (fn k)

  (* *)

  let rec freshen env =
    keep_phys_eq @@ function
    | `Star _ as inn -> inn
    | `Arrow (at', d, c) -> `Arrow (at', freshen env d, freshen env c)
    | `Unk (at', i) -> (
      match UnkMap.find_opt i !env with
      | None ->
        let v' = fresh at' in
        env := UnkMap.add i v' !env;
        v'
      | Some k -> k)

  (* Formatting *)

  module Numbering = struct
    include UnkMap

    type nonrec t = int t ref

    let create () = ref empty
  end

  let pp ?(numbering = Numbering.create ()) kind =
    let rec pp atomize kind =
      let open FomPP in
      match kind with
      | `Star _ -> star
      | `Arrow (_, dom, cod) ->
        let dom = pp true dom in
        let cod = pp false cod in
        dom ^^ space_arrow_right_break_1 ^^ cod
        |> if atomize then egyptian parens 2 else Fun.id
      | `Unk (_, i) ->
        let n =
          match UnkMap.find_opt i !numbering with
          | None ->
            let n = UnkMap.cardinal !numbering in
            numbering := UnkMap.add i n !numbering;
            n
          | Some n -> n
        in
        kappa_lower ^^ subscript n
    in
    pp false kind |> FomPP.group

  let pp_annot ?(numbering = Numbering.create ()) = function
    | `Star _ -> empty
    | kind -> colon ^^ align (pp ~numbering kind)
end

module Label = struct
  include Id.Make ()

  (** If both labels are numeric, comparison is done by numeric value. *)
  let compare lhs rhs =
    if lhs == rhs then
      0
    else
      let lhs = to_string lhs in
      let rhs = to_string rhs in
      try int_of_string lhs - int_of_string rhs
      with Failure _ -> String.compare lhs rhs
end

module Row = struct
  type 't t = (Label.t * 't) list

  let is_tuple labels =
    labels
    |> List.for_alli @@ fun i (l, _) -> Label.to_string l = Int.to_string (i + 1)

  let map fn = List.map (Pair.map Fun.id fn)
  let map_phys_eq fn = List.map_phys_eq (Pair.map_phys_eq Fun.id fn)
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
      if lhs == rhs then
        0
      else
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

  module Var = Id.Make ()

  type ('t, 'k) f =
    [ `Mu of Loc.t * 't
    | `Const of Loc.t * Const.t
    | `Var of Loc.t * Var.t
    | `Lam of Loc.t * Var.t * 'k * 't
    | `App of Loc.t * 't * 't
    | `ForAll of Loc.t * 't
    | `Exists of Loc.t * 't
    | `Arrow of Loc.t * 't * 't
    | `Product of Loc.t * 't Row.t
    | `Sum of Loc.t * 't Row.t ]

  type t = (t, Kind.t) f

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

  let var v = `Var (Var.at v, v)
  let sort labels = List.sort (Compare.the fst Label.compare) labels
  let product at fs = `Product (at, sort fs)
  let sum at cs = `Sum (at, sort cs)
  let zero at = `Sum (at, [])

  (* Type predicates *)

  let is_int = function `Const (_, `Int) -> true | _ -> false

  (* Type applications *)

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

  (* *)

  let eq l r =
    match (l, r) with
    | `Mu l, `Mu r -> eq'2 l r
    | `Const l, `Const r -> eq'2 l r
    | `Var l, `Var r -> eq'2 l r
    | `Lam l, `Lam r -> eq'4 l r
    | `App l, `App r -> eq'3 l r
    | `ForAll l, `ForAll r -> eq'2 l r
    | `Exists l, `Exists r -> eq'2 l r
    | `Arrow l, `Arrow r -> eq'3 l r
    | `Product l, `Product r -> eq'2 l r
    | `Sum l, `Sum r -> eq'2 l r
    | _ -> false

  (* *)

  let keep_phys_eq' t t' = if t == t' || eq t t' then t else t'
  let keep_phys_eq fn t = keep_phys_eq' t (fn t)

  (* Substitution *)

  module VarSet = Set.Make (Var)

  let free' free = function
    | `Const _ -> VarSet.empty
    | `Var (_, i) -> VarSet.singleton i
    | `Lam (_, i, _, e) -> free e |> VarSet.remove i
    | `App (_, f, x) -> VarSet.union (free f) (free x)
    | `Mu (_, e) | `ForAll (_, e) | `Exists (_, e) -> free e
    | `Arrow (_, d, c) -> VarSet.union (free d) (free c)
    | `Product (_, ls) | `Sum (_, ls) ->
      List.fold_left (fun s (_, t) -> VarSet.union s (free t)) VarSet.empty ls

  let rec free t = free' free t
  let free = Profiling.Counter.wrap'1 "free" free

  module VarMap = Map.Make (Var)

  let impure = Var.of_string Loc.dummy "impure"

  let initial_env =
    let star = `Star Loc.dummy in
    let arrow d c = `Arrow (Loc.dummy, d, c) in
    [(impure, (impure, arrow star star))] |> List.to_seq |> VarMap.of_seq

  let rec subst_rec env =
    keep_phys_eq @@ function
    | `Mu (at, t) -> `Mu (at, subst_rec env t)
    | `Const _ as inn -> inn
    | `Var (_, i) as inn -> (
      match VarMap.find_opt i env with None -> inn | Some t -> subst_rec env t)
    | `Lam (at, i, k, t) as inn ->
      let env = VarMap.remove i env in
      if VarMap.is_empty env then inn else `Lam (at, i, k, subst_rec env t)
    | `App (at, f, x) -> `App (at, subst_rec env f, subst_rec env x)
    | `ForAll (at, t) -> `ForAll (at, subst_rec env t)
    | `Exists (at, t) -> `Exists (at, subst_rec env t)
    | `Arrow (at, d, c) -> `Arrow (at, subst_rec env d, subst_rec env c)
    | `Product (at, ls) -> `Product (at, Row.map_phys_eq (subst_rec env) ls)
    | `Sum (at, ls) -> `Sum (at, Row.map_phys_eq (subst_rec env) ls)

  let subst_rec env t = if VarMap.is_empty env then t else subst_rec env t

  let is_free' is_free id = function
    | `Const _ -> false
    | `Var (_, id') -> Var.equal id id'
    | `Lam (_, id', _, body) -> (not (Var.equal id id')) && is_free id body
    | `App (_, fn, arg) -> is_free id fn || is_free id arg
    | `Mu (_, typ) | `ForAll (_, typ) | `Exists (_, typ) -> is_free id typ
    | `Arrow (_, d, c) -> is_free id d || is_free id c
    | `Product (_, ls) | `Sum (_, ls) ->
      ls |> List.exists @@ fun (_, t) -> is_free id t

  let rec is_free id = is_free' is_free id
  let is_free = Profiling.Counter.wrap'2 "is_free" is_free

  let mu_of_norm' is_free at = function
    | `Lam (_, i, _, t) when not (is_free i t) -> t
    | t' -> `Mu (at, t')

  let lam_of_norm' is_free at i k = function
    | `App (_, f, `Var (_, i')) when Var.equal i i' && not (is_free i f) -> f
    | t' -> `Lam (at, i, k, t')

  let app_of_norm' subst_of_norm at f' x' =
    match f' with
    | `Lam (_, i, _, t) -> subst_of_norm (VarMap.singleton i x') t
    | f' -> `App (at, f', x')

  let subst_of_norm' subst_of_norm is_free env = function
    | `Var (_, i) as inn -> (
      match VarMap.find_opt i env with None -> inn | Some t -> t)
    | `Mu (at, t) -> mu_of_norm' is_free at (subst_of_norm env t)
    | `Lam (at, i, k, t) as inn ->
      let env = VarMap.remove i env in
      if VarMap.is_empty env then
        inn
      else if VarMap.exists (fun i' t' -> is_free i t' && is_free i' t) env then
        let i' = Var.freshen i in
        let v' = `Var (at, i') in
        let t' = subst_of_norm (VarMap.add i v' env) t in
        lam_of_norm' is_free at i' k t'
      else
        lam_of_norm' is_free at i k (subst_of_norm env t)
    | `App (at, f, x) ->
      app_of_norm' subst_of_norm at (subst_of_norm env f) (subst_of_norm env x)
    | `Const _ as inn -> inn
    | `ForAll (at, t) -> `ForAll (at, subst_of_norm env t)
    | `Exists (at, t) -> `Exists (at, subst_of_norm env t)
    | `Arrow (at, d, c) -> `Arrow (at, subst_of_norm env d, subst_of_norm env c)
    | `Product (at, ls) -> `Product (at, Row.map_phys_eq (subst_of_norm env) ls)
    | `Sum (at, ls) -> `Sum (at, Row.map_phys_eq (subst_of_norm env) ls)

  let rec subst_of_norm env =
    keep_phys_eq @@ subst_of_norm' subst_of_norm is_free env

  let subst_of_norm env t =
    if VarMap.is_empty env then t else subst_of_norm env t

  let subst_of_norm = Profiling.Counter.wrap'2 "subst_of_norm" subst_of_norm
  let mu_of_norm at = mu_of_norm' is_free at
  let lam_of_norm at = lam_of_norm' is_free at
  let app_of_norm at = app_of_norm' subst_of_norm at
  let apps_of_norm at = List.fold_left @@ app_of_norm at

  (* Freshening *)

  let freshen (t : t) : t =
    let env = ref Kind.UnkMap.empty in
    let rec freshen t =
      t
      |> keep_phys_eq @@ function
         | `Mu (at, t) -> `Mu (at, freshen t)
         | (`Const _ | `Var _) as inn -> inn
         | `Lam (at, i, k, t) -> `Lam (at, i, Kind.freshen env k, freshen t)
         | `App (at, f, x) -> `App (at, freshen f, freshen x)
         | `ForAll (at, t) -> `ForAll (at, freshen t)
         | `Exists (at, t) -> `Exists (at, freshen t)
         | `Arrow (at, d, c) -> `Arrow (at, freshen d, freshen c)
         | `Product (at, ls) -> `Product (at, Row.map_phys_eq freshen ls)
         | `Sum (at, ls) -> `Sum (at, Row.map_phys_eq freshen ls)
    in
    freshen t

  let freshen = Profiling.Counter.wrap'1 "freshen" freshen

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

  let compare' compare env lhs rhs =
    if lhs == rhs then
      0
    else
      match (lhs, rhs) with
      | `Mu (_, lhs), `Mu (_, rhs) -> compare env lhs rhs
      | `Const (_, lhs), `Const (_, rhs) -> Const.compare lhs rhs
      | `Var (_, lhs), `Var (_, rhs) ->
        let lhs = VarMap.find_opt lhs env |> Option.value ~default:lhs in
        let rhs = VarMap.find_opt rhs env |> Option.value ~default:rhs in
        Var.compare lhs rhs
      | `Lam (_, lhs_id, lhs_kind, lhs_typ), `Lam (_, rhs_id, rhs_kind, rhs_typ)
        ->
        Kind.compare lhs_kind rhs_kind <>? fun () ->
        if Var.equal lhs_id rhs_id then
          compare env lhs_typ rhs_typ
        else
          let v = Var.fresh Loc.dummy in
          let env = env |> VarMap.add lhs_id v |> VarMap.add rhs_id v in
          compare env lhs_typ rhs_typ
      | `App (_, lhs_fn, lhs_arg), `App (_, rhs_fn, rhs_arg) ->
        compare env lhs_arg rhs_arg <>? fun () -> compare env lhs_fn rhs_fn
      | `ForAll (_, lhs), `ForAll (_, rhs) | `Exists (_, lhs), `Exists (_, rhs)
        ->
        compare env lhs rhs
      | `Arrow (_, lhs_d, lhs_c), `Arrow (_, rhs_d, rhs_c) ->
        compare env lhs_d rhs_d <>? fun () -> compare env lhs_c rhs_c
      | `Product (_, lhs_ls), `Product (_, rhs_ls)
      | `Sum (_, lhs_ls), `Sum (_, rhs_ls) ->
        List.compare_with
          (fun (lhs_l, lhs_t) (rhs_l, rhs_t) ->
            Label.compare lhs_l rhs_l <>? fun () -> compare env lhs_t rhs_t)
          lhs_ls rhs_ls
      | _ -> index lhs - index rhs

  let rec compare env lhs rhs = compare' compare env lhs rhs
  let compare = Profiling.Counter.wrap'2 "compare" (compare VarMap.empty)

  (* Formatting *)

  let prec_min = 0
  let prec_arrow = 1
  let prec_app = 2
  let prec_max = 3

  (* *)

  let some_spaces = Some spaces

  let rec hanging = function
    | `Lam _ | `Mu (_, `Lam _) | `ForAll (_, `Lam _) | `Exists (_, `Lam _) ->
      some_spaces
    | `Product _ -> some_spaces
    | `App _ as t -> (
      match unapp t with `Var _, [x] -> hanging x | _ -> None)
    | _ -> None

  let rec binding pp_annot prec_outer head i k t =
    (group (head ^^ Var.pp i ^^ pp_annot k ^^ dot |> nest 2)
    ^^
    match hanging t with
    | Some _ -> pp pp_annot prec_min t
    | None -> break_0 ^^ group (pp pp_annot prec_min t) |> nest 2 |> group)
    |> if prec_min < prec_outer then egyptian parens 2 else Fun.id

  and quantifier pp_annot prec_outer symbol (typ : t) =
    match typ with
    | `Lam (_, id, kind, body) ->
      binding pp_annot prec_outer symbol id kind body
    | _ -> symbol ^^ egyptian parens 2 (pp pp_annot prec_min typ)

  and labeled pp_annot labels =
    labels
    |> List.stable_sort (Compare.the (fst >>> Label.at >>> fst) Pos.compare)
    |> List.map (function
         | l, `Var (_, i) when Var.name i = Label.name l -> Label.pp l
         | label, typ -> (
           Label.pp label ^^ colon
           ^^
           match hanging typ with
           | Some (lhs, _) -> lhs ^^ pp pp_annot prec_min typ
           | None -> break_1 ^^ pp pp_annot prec_min typ |> nest 2 |> group))
    |> separate comma_break_1_or_break_0

  and ticked pp_annot labels =
    match
      labels
      |> List.stable_sort (Compare.the (fst >>> Label.at >>> fst) Pos.compare)
      |> List.map @@ function
         | l, `Product (_, []) -> tick ^^ Label.pp l
         | l, t ->
           tick ^^ Label.pp l ^^ break_1 ^^ pp pp_annot prec_max t
           |> nest 2 |> group
    with
    | [l] -> l
    | [] -> pipe
    | ls ->
      ls |> separate break_1_pipe_space |> precede (ifflat empty pipe_space)

  and tuple pp_annot labels =
    labels |> List.map (snd >>> pp pp_annot prec_min) |> separate comma_break_1

  and pp pp_annot prec_outer (typ : t) =
    match typ with
    | `Const (_, const) -> Const.pp const
    | `Var (_, id) -> Var.pp id
    | `Lam (_, id, kind, body) ->
      binding pp_annot prec_outer lambda_lower id kind body
    | `Mu (_, typ) -> quantifier pp_annot prec_outer FomPP.mu_lower typ
    | `ForAll (_, typ) -> quantifier pp_annot prec_outer FomPP.for_all typ
    | `Exists (_, typ) -> quantifier pp_annot prec_outer FomPP.exists typ
    | `Arrow (_, dom, cod) ->
      pp pp_annot (prec_arrow + 1) dom
      ^^ (match hanging cod with
         | Some (lhs, _) -> space_arrow_right ^^ lhs
         | None -> space_arrow_right_break_1)
      ^^ pp pp_annot (prec_arrow - 1) cod
      |> if prec_arrow < prec_outer then egyptian parens 2 else Fun.id
    | `Product (_, labels) ->
      if Row.is_tuple labels then
        tuple pp_annot labels |> egyptian parens 2
      else
        labeled pp_annot labels |> egyptian braces 2
    | `Sum (_, [(l, `Product (_, []))]) -> tick ^^ Label.pp l
    | `Sum (_, labels) ->
      ticked pp_annot labels
      |> if prec_arrow < prec_outer then egyptian parens 2 else Fun.id
    | `App (_, _, _) -> (
      match unapp typ with
      | f, xs ->
        pp pp_annot prec_app f
        :: (xs |> List.map (pp pp_annot (prec_app + 1) >>> group))
        |> separate break_1
        |> if prec_app < prec_outer then egyptian parens 2 else group)

  let pp ?(pp_annot = Kind.pp_annot ~numbering:(Kind.Numbering.create ())) typ =
    pp pp_annot prec_min typ |> group
end

module Exp = struct
  let bool = `Const (Loc.dummy, `Bool)
  let int = `Const (Loc.dummy, `Int)
  let string = `Const (Loc.dummy, `String)
  let impure = `Var (Loc.dummy, Typ.impure)

  module Const = struct
    type ('nat, 't) t =
      [ `LitBool of bool
      | `LitNat of 'nat
      | `LitString of JsonString.t
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
      | `OpLogicalOr
      | `OpStringCat
      | `Keep of 't
      | `Target of 't * JsonString.t ]

    (* Typing *)

    let type_of at =
      let bpr t = `Arrow (at, t, `Arrow (at, t, bool)) in
      let uop t = `Arrow (at, t, t) in
      let bop t = `Arrow (at, t, uop t) in
      function
      | `LitBool _ -> `Const (at, `Bool)
      | `LitNat _ -> `Const (at, `Int)
      | `LitString _ -> `Const (at, `String)
      | `OpArithAdd | `OpArithSub | `OpArithMul | `OpArithDiv | `OpArithRem ->
        bop int
      | `OpArithPlus | `OpArithMinus -> uop int
      | `OpCmpLt | `OpCmpLtEq | `OpCmpGt | `OpCmpGtEq -> bpr int
      | `OpEq typ | `OpEqNot typ -> bpr typ
      | `OpLogicalAnd | `OpLogicalOr -> bop bool
      | `OpLogicalNot -> uop bool
      | `Keep t -> `Arrow (at, `App (at, impure, t), t)
      | `OpStringCat -> bop string
      | `Target (t, _) -> t

    (* Substitution *)

    let map_typ tu = function
      | ( `LitBool _ | `LitNat _ | `LitString _ | `OpArithAdd | `OpArithDiv
        | `OpArithMinus | `OpArithMul | `OpArithPlus | `OpArithRem | `OpArithSub
        | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpLogicalAnd
        | `OpLogicalNot | `OpLogicalOr | `OpStringCat ) as c ->
        c
      | `OpEq t -> `OpEq (tu t)
      | `OpEqNot t -> `OpEqNot (tu t)
      | `Keep t -> `Keep (tu t)
      | `Target (t, l) -> `Target (tu t, l)

    let traverse_typ tuM =
      let open Rea in
      function
      | ( `LitBool _ | `LitNat _ | `LitString _ | `OpArithAdd | `OpArithDiv
        | `OpArithMinus | `OpArithMul | `OpArithPlus | `OpArithRem | `OpArithSub
        | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpLogicalAnd
        | `OpLogicalNot | `OpLogicalOr | `OpStringCat ) as c ->
        return c
      | `OpEq t -> tuM t >>- fun t -> `OpEq t
      | `OpEqNot t -> tuM t >>- fun t -> `OpEqNot t
      | `Keep t -> tuM t >>- fun t -> `Keep t
      | `Target (t, l) -> tuM t >>- fun t -> `Target (t, l)

    let collect_typ = function
      | `LitBool _ | `LitNat _ | `LitString _ | `OpArithAdd | `OpArithDiv
      | `OpArithMinus | `OpArithMul | `OpArithPlus | `OpArithRem | `OpArithSub
      | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpLogicalAnd
      | `OpLogicalNot | `OpLogicalOr | `OpStringCat ->
        []
      | `OpEq t | `OpEqNot t | `Keep t | `Target (t, _) -> [t]

    let lit_false = `LitBool false
    let lit_true = `LitBool true

    (* Comparison *)

    let index = function
      | `LitBool _ -> 0
      | `LitNat _ -> 1
      | `LitString _ -> 2
      | `OpArithAdd -> 3
      | `OpArithDiv -> 4
      | `OpArithMinus -> 5
      | `OpArithMul -> 6
      | `OpArithPlus -> 7
      | `OpArithRem -> 8
      | `OpArithSub -> 9
      | `OpCmpGt -> 10
      | `OpCmpGtEq -> 11
      | `OpCmpLt -> 12
      | `OpCmpLtEq -> 13
      | `OpEq _ -> 14
      | `OpEqNot _ -> 15
      | `OpLogicalAnd -> 16
      | `OpLogicalNot -> 17
      | `OpLogicalOr -> 18
      | `OpStringCat -> 19
      | `Keep _ -> 20
      | `Target _ -> 21

    let compare' nat typ l r =
      match (l, r) with
      | `LitBool l, `LitBool r -> Bool.compare l r
      | `LitNat l, `LitNat r -> nat l r
      | `LitString l, `LitString r -> JsonString.compare l r
      | `OpEq l, `OpEq r | `OpEqNot l, `OpEqNot r -> typ l r
      | `Keep tl, `Keep tr -> typ tl tr
      | `Target (tl, ll), `Target (tr, lr) ->
        typ tl tr <>? fun () -> JsonString.compare ll lr
      | _ -> index l - index r

    (* Formatting *)

    let pp' nat typ = function
      | `LitBool bool -> if bool then true' else false'
      | `LitNat i -> nat i
      | `LitString s -> utf8string @@ JsonString.to_utf8_json s
      | `OpArithAdd -> plus
      | `OpArithDiv -> slash
      | `OpArithMinus -> minus
      | `OpArithMul -> star
      | `OpArithPlus -> plus
      | `OpArithRem -> percent
      | `OpArithSub -> minus
      | `OpCmpGt -> rangle
      | `OpCmpGtEq -> greater_equal
      | `OpCmpLt -> langle
      | `OpCmpLtEq -> less_equal
      | `OpEq t -> equals ^^ egyptian brackets 2 (typ t)
      | `OpEqNot t -> not_equal ^^ egyptian brackets 2 (typ t)
      | `OpLogicalAnd -> logical_and
      | `OpLogicalNot -> logical_not
      | `OpLogicalOr -> logical_or
      | `OpStringCat -> caret
      | `Keep t -> keep' ^^ egyptian brackets 2 (typ t)
      | `Target (t, l) ->
        target'
        ^^ egyptian brackets 2 (typ t)
        ^^ space ^^ utf8string @@ JsonString.to_utf8_json l

    let pp = pp' (Bigint.to_string >>> utf8string) Typ.pp
  end

  module Var = Id.Make ()
  module VarSet = Set.Make (Var)
  module VarMap = Map.Make (Var)

  type ('e, 't, 'k) f =
    [ `Const of Loc.t * (Bigint.t, 't) Const.t
    | `Var of Loc.t * Var.t
    | `Lam of Loc.t * Var.t * 't * 'e
    | `App of Loc.t * 'e * 'e
    | `Gen of Loc.t * Typ.Var.t * 'k * 'e
    | `Inst of Loc.t * 'e * 't
    | `LetIn of Loc.t * Var.t * 'e * 'e
    | `Mu of Loc.t * 'e
    | `IfElse of Loc.t * 'e * 'e * 'e
    | `Product of Loc.t * 'e Row.t
    | `Select of Loc.t * 'e * 'e
    | `Inject of Loc.t * Label.t * 'e
    | `Case of Loc.t * 'e
    | `Pack of Loc.t * 't * 'e * 't
    | `UnpackIn of Loc.t * Typ.Var.t * Var.t * 'e * 'e ]

  type t = (t, Typ.t, Kind.t) f

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

  let builtins =
    let at = Loc.dummy in
    [
      (Var.of_string at "true", `Const (at, `LitBool true));
      (Var.of_string at "false", `Const (at, `LitBool false));
      ( Var.of_string at "keep",
        let t = Typ.Var.fresh at in
        `Gen (at, t, `Star at, `Const (at, `Keep (Typ.var t))) );
    ]

  let initial_exp e =
    builtins |> List.fold_left (fun e (i, v) -> `LetIn (Loc.dummy, i, v, e)) e
end
