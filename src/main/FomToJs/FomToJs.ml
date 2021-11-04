open FomPP
open FomBasis
open FomSource
open FomAST

(* *)

open Cats

module Label = struct
  include Label

  let is_numeric id =
    let it = to_string id in
    '0' <= it.[0] && it.[0] <= '9'

  let to_str = to_string >>> str

  (* *)

  let to_js_label = to_str

  let to_js_select l =
    if is_numeric l then str "[" ^ to_str l ^ str "]" else str "." ^ to_str l

  let to_js_atom l =
    if is_numeric l then to_str l else str "\"" ^ to_str l ^ str "\""
end

module Erased = struct
  type t =
    [ `App of t * t
    | `Case of t
    | `Const of (int32, Typ.Core.t) Exp.Const.t
    | `IfElse of t * t * t
    | `Inject of Label.t * t
    | `Lam of Exp.Var.t * t
    | `Mu of t
    | `Product of (Label.t * t) list
    | `Select of t * t
    | `Var of Exp.Var.t ]
end

module Exp = struct
  include Exp

  module Const = struct
    include Const

    let bi_2_pow_31, bi_2_pow_32, bi_2_pow_32_minus_1 =
      let open Bigint in
      ( shift_left (of_int 1) 31,
        shift_left (of_int 1) 32,
        shift_left (of_int 1) 32 - of_int 1 )

    let to_js = function
      | `LitBool bool -> Bool.to_string bool |> str
      | `LitNat nat -> Int32.to_string nat |> str
      | `LitString lit -> str @@ JsonString.to_utf8_json lit
      | `OpArithAdd -> str "+"
      | `OpArithDiv -> str "/"
      | `OpArithMinus -> str "-"
      | `OpArithMul -> str "*"
      | `OpArithPlus -> str "+"
      | `OpArithRem -> str "%"
      | `OpArithSub -> str "-"
      | `OpCmpGt -> str ">"
      | `OpCmpGtEq -> str ">="
      | `OpCmpLt -> str "<"
      | `OpCmpLtEq -> str "<="
      | `OpEq _ -> str "==="
      | `OpEqNot _ -> str "!=="
      | `OpLogicalAnd -> str "&&"
      | `OpLogicalNot -> str "!"
      | `OpLogicalOr -> str "||"
      | `OpStringCat -> str "+"
      | `Keep _ -> str ""
      | `Target (_, l) -> str "(" ^ str (JsonString.to_utf8 l) ^ str ")"

    let is_total = function
      | `Keep _ -> false
      | `LitBool _ | `LitNat _ | `LitString _ | `OpArithAdd | `OpArithDiv
      | `OpArithMinus | `OpArithMul | `OpArithPlus | `OpArithRem | `OpArithSub
      | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpEq _ | `OpEqNot _
      | `OpLogicalAnd | `OpLogicalNot | `OpLogicalOr | `OpStringCat | `Target _
        ->
        true

    let is_uop = function
      | `OpArithPlus | `OpArithMinus | `OpLogicalNot | `Keep _ -> true
      | `LitBool _ | `LitNat _ | `LitString _ | `OpArithAdd | `OpArithDiv
      | `OpArithMul | `OpArithRem | `OpArithSub | `OpCmpGt | `OpCmpGtEq
      | `OpCmpLt | `OpCmpLtEq | `OpEq _ | `OpEqNot _ | `OpLogicalAnd
      | `OpLogicalOr | `OpStringCat | `Target _ ->
        false

    let is_bop = function
      | `LitBool _ | `LitNat _ | `LitString _ | `Target _ | `OpArithMinus
      | `OpArithPlus | `OpLogicalNot | `Keep _ ->
        false
      | `OpArithAdd | `OpArithDiv | `OpArithMul | `OpArithRem | `OpArithSub
      | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpEq _ | `OpEqNot _
      | `OpLogicalAnd | `OpLogicalOr | `OpStringCat ->
        true

    let erase = function
      | `LitNat nat ->
        let open Bigint in
        let nat = bit_and nat bi_2_pow_32_minus_1 in
        (* TODO: Warn when literal is truncated. *)
        `LitNat
          (Int32.of_string
             (if nat < bi_2_pow_31 then
                to_string nat
             else
               nat - bi_2_pow_32 |> to_string))
      | ( `LitBool _ | `LitString _ | `OpArithAdd | `OpArithDiv | `OpArithMinus
        | `OpArithMul | `OpArithPlus | `OpArithRem | `OpArithSub | `OpCmpGt
        | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq | `OpEq _ | `OpEqNot _
        | `OpLogicalAnd | `OpLogicalNot | `OpLogicalOr | `OpStringCat | `Keep _
        | `Target _ ) as other ->
        other

    let is_commutative = function
      | `OpArithAdd | `OpArithMul | `OpEq _ | `OpEqNot _ -> true
      | `LitBool _ | `LitNat _ | `LitString _ | `OpArithDiv | `OpArithMinus
      | `OpArithPlus | `OpArithRem | `OpArithSub | `OpCmpGt | `OpCmpGtEq
      | `OpCmpLt | `OpCmpLtEq | `OpLogicalAnd | `OpLogicalNot | `OpLogicalOr
      | `OpStringCat | `Keep _ | `Target _ ->
        false

    let simplify_uop = function
      (* + *)
      | `OpArithPlus, x -> Some x
      (* - *)
      | `OpArithMinus, `Const (`LitNat v) ->
        Some (`Const (`LitNat (Int32.neg v)))
      | `OpArithMinus, `App (`Const `OpArithMinus, x) -> Some x
      (* ! *)
      | `OpLogicalNot, `App (`Const `OpLogicalNot, x) -> Some x
      | `OpLogicalNot, `Const (`LitBool v) -> Some (`Const (`LitBool (not v)))
      | _ -> None

    let compare l r = compare' Int32.compare (Typ.compare :> Typ.Core.t cmp) l r

    (* TODO: More comprehensive constant folding rules *)
    let simplify_bop = function
      (* + *)
      | `OpArithAdd, `Const (`LitNat x), `Const (`LitNat y) ->
        Some (`Const (`LitNat (Int32.add x y)))
      | ( `OpArithAdd,
          `App (`App (`Const `OpArithAdd, x), `Const (`LitNat y)),
          `Const (`LitNat z) ) ->
        Some
          (`App
            (`App (`Const `OpArithAdd, x), `Const (`LitNat (Int32.add y z))))
      | `OpArithAdd, (`Const _ as c), x ->
        Some (`App (`App (`Const `OpArithAdd, x), c))
      | `OpArithAdd, `App (`App (`Const `OpArithAdd, x), (`Const _ as c)), y ->
        Some
          (`App
            ( `App (`Const `OpArithAdd, `App (`App (`Const `OpArithAdd, x), y)),
              c ))
      | `OpArithAdd, x, `Const (`LitNat 0l) -> Some x
      | `OpArithAdd, x, `App (`Const `OpArithMinus, y) ->
        Some (`App (`App (`Const `OpArithSub, x), y))
      | `OpArithAdd, `Var i, `Var j when Var.equal i j ->
        Some (`App (`App (`Const `OpArithMul, `Const (`LitNat 2l)), `Var i))
      | `OpArithAdd, x, `Const (`LitNat y)
        when y < Int32.of_int 0 && y <> Int32.min_int ->
        Some
          (`App (`App (`Const `OpArithSub, x), `Const (`LitNat (Int32.neg y))))
      (* / *)
      | `OpArithDiv, _, `Const (`LitNat 0l)
      | `OpArithDiv, `Const (`LitNat 0l), _ ->
        Some (`Const (`LitNat 0l))
      | `OpArithDiv, `Const (`LitNat x), `Const (`LitNat y) ->
        Some (`Const (`LitNat (Int32.div x y)))
      | `OpArithDiv, x, `Const (`LitNat 1l) -> Some x
      | `OpArithDiv, x, `Const (`LitNat -1l) ->
        Some (`App (`Const `OpArithMinus, x))
      (* * *)
      | `OpArithMul, `Const (`LitNat x), `Const (`LitNat y) ->
        Some (`Const (`LitNat (Int32.mul x y)))
      | `OpArithMul, x, `Const (`LitNat 1l)
      | `OpArithMul, `Const (`LitNat 1l), x ->
        Some x
      | `OpArithMul, x, `Const (`LitNat -1l)
      | `OpArithMul, `Const (`LitNat -1l), x ->
        Some (`App (`Const `OpArithMinus, x))
      | `OpArithMul, _, `Const (`LitNat 0l)
      | `OpArithMul, `Const (`LitNat 0l), _ ->
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
      | `OpEq _, `Const x, `Const y ->
        Some (`Const (`LitBool (compare x y = 0)))
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

  module Var = struct
    include Var

    let to_js id =
      let id = to_string id in
      if Js.is_illegal_id id then
        str "$" ^ str id ^ str "$"
      else
        str id
  end

  let rec is_free i' = function
    | `Const _ -> false
    | `Var i -> Var.equal i' i
    | `Lam (i, e) -> (not (Var.equal i' i)) && is_free i' e
    | `App (f, x) -> is_free i' f || is_free i' x
    | `IfElse (c, t, e) -> is_free i' c || is_free i' t || is_free i' e
    | `Product fs -> fs |> List.exists (snd >>> is_free i')
    | `Mu e | `Inject (_, e) | `Case e -> is_free i' e
    | `Select (e, l) -> is_free i' e || is_free i' l

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

  let rec subst i the =
    keep_phys_eq @@ function
    | `Const _ as inn -> inn
    | `Var i' as inn -> if Var.equal i' i then the else inn
    | `Lam (i', e) as inn ->
      if Var.equal i' i || not (is_free i e) then
        inn
      else if is_free i' the then
        let i'' = Var.freshen i' in
        let vi'' = `Var i'' in
        `Lam (i'', subst i the (subst i' vi'' e))
      else
        `Lam (i', subst i the e)
    | `App (f, x) -> `App (subst i the f, subst i the x)
    | `Mu e -> `Mu (subst i the e)
    | `IfElse (c, t, e) -> `IfElse (subst i the c, subst i the t, subst i the e)
    | `Product fs -> `Product (Row.map_phys_eq (subst i the) fs)
    | `Select (e, l) -> `Select (subst i the e, subst i the l)
    | `Inject (l, e) -> `Inject (l, subst i the e)
    | `Case cs -> `Case (subst i the cs)

  module Erased = struct
    include Erased

    let index = function
      | `App _ -> 0
      | `Case _ -> 1
      | `Const _ -> 2
      | `IfElse _ -> 3
      | `Inject _ -> 4
      | `Lam _ -> 5
      | `Mu _ -> 6
      | `Product _ -> 7
      | `Select _ -> 8
      | `Var _ -> 9

    let rec compare (l : Erased.t) (r : Erased.t) =
      if l == r then
        0
      else
        match (l, r) with
        | `App (fl, xl), `App (fr, xr) ->
          compare xl xr <>? fun () -> compare fl fr
        | `Case l, `Case r | `Mu l, `Mu r -> compare l r
        | `Const l, `Const r ->
          Exp.Const.compare' Int32.compare (Typ.compare :> Typ.Core.t cmp) l r
        | `IfElse (cl, tl, el), `IfElse (cr, tr, er) ->
          compare cl cr <>? fun () ->
          compare tl tr <>? fun () -> compare el er
        | `Select (el, ll), `Select (er, lr) ->
          compare ll lr <>? fun () -> compare el er
        | `Inject (ll, el), `Inject (lr, er) ->
          Label.compare ll lr <>? fun () -> compare el er
        | `Lam (vl, el), `Lam (vr, er) ->
          if Var.equal vl vr then
            compare el er
          else
            let v = `Var (Var.fresh Loc.dummy) in
            compare (subst vl v el) (subst vr v er)
        | `Product lls, `Product rls ->
          List.compare_with
            (fun (ll, el) (lr, er) ->
              Label.compare ll lr <>? fun () -> compare el er)
            lls rls
        | `Var l, `Var r -> Var.compare l r
        | _ -> index l - index r

    let[@warning "-32"] rec pp : t -> document = function
      | `App (f, x) -> [pp f; space; pp x] |> concat |> egyptian parens 2
      | `Case t -> [utf8string "case"; space; pp t] |> concat
      | `Const c -> Exp.Const.pp' (Int32.to_string >>> utf8string) Typ.pp c
      | `IfElse (c, t, e) ->
        [
          [utf8string "if"; space; pp c; space] |> concat;
          [utf8string "then"; space; pp t; space] |> concat;
          [utf8string "else"; space; pp e; space] |> concat;
        ]
        |> concat |> egyptian parens 2
      | `Inject (l, t) ->
        [tick; Label.pp l; break_1; pp t] |> concat |> egyptian parens 2
      | `Lam (v, t) ->
        [lambda_lower; Var.pp v; dot; pp t] |> concat |> egyptian parens 2
      | `Mu t -> [mu_lower; pp t |> egyptian parens 2] |> concat
      | `Product ls ->
        ls
        |> List.map (fun (l, t) -> [Label.pp l; equals; pp t] |> concat)
        |> separate comma_break_1 |> egyptian braces 2
      | `Select (t, `Inject (l, `Product [])) ->
        [pp t; dot; Label.pp l] |> concat
      | `Select (t, l) -> [pp t; dot; pp l |> egyptian parens 2] |> concat
      | `Var v -> Var.pp v

    let[@warning "-32"] to_string = pp >>> FomPP.to_string
  end

  let coerce_to_int exp = str "(" ^ exp ^ str ") | 0"
  let coerce_to_int_if bool exp = if bool then coerce_to_int exp else exp
  let parens exp = str "(" ^ exp ^ str ")"

  module VarMap = struct
    include Map.Make (Var)

    type nonrec t = Erased.t t

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
    type t = int option

    let field r = r#limit

    class con =
      object
        val limit : t = None
        method limit = Field.make limit (fun v -> {<limit = v>})
      end
  end

  module Seen = struct
    include Set.Make (Erased)

    let field r = r#seen
    let adding e = mapping field (add e)

    class con =
      object
        val seen : t = empty
        method seen = Field.make seen (fun v -> {<seen = v>})
      end
  end

  module VarSet = Set.Make (Var)

  let rec erase = function
    | `Const (_, c) -> `Const (Const.erase c)
    | `Var (_, i) -> `Var i
    | `Lam (_, i, _, e) -> `Lam (i, erase e)
    | `App (_, f, x) -> `App (erase f, erase x)
    | `UnpackIn (_, _, _, i, v, e) -> `App (`Lam (i, erase e), erase v)
    | `Mu (_, e) -> `Mu (erase e)
    | `IfElse (_, c, t, e) -> `IfElse (erase c, erase t, erase e)
    | `Product (_, fs) -> `Product (fs |> Row.map erase)
    | `Select (_, e, l) -> `Select (erase e, erase l)
    | `Inject (_, l, e) -> `Inject (l, erase e)
    | `Case (_, cs) -> `Case (erase cs)
    | `Gen (_, _, _, e) | `Inst (_, e, _) | `Pack (_, _, e, _) -> erase e

  let rec bottomUp fn = function
    | (`Const _ | `Var _) as e -> fn e
    | `App (f, x) -> fn (`App (bottomUp fn f, bottomUp fn x))
    | `IfElse (c, t, e) ->
      fn (`IfElse (bottomUp fn c, bottomUp fn t, bottomUp fn e))
    | `Product fs -> fn (`Product (fs |> Row.map (bottomUp fn)))
    | `Mu e -> fn (`Mu (bottomUp fn e))
    | `Lam (i, e) -> fn (`Lam (i, bottomUp fn e))
    | `Inject (l, e) -> fn (`Inject (l, bottomUp fn e))
    | `Select (e, l) -> fn (`Select (bottomUp fn e, bottomUp fn l))
    | `Case cs -> fn (`Case (bottomUp fn cs))

  let size =
    bottomUp @@ function
    | `Const _ | `Var _ -> 1
    | `App (f, x) -> f + x + 1
    | `IfElse (c, t, e) -> c + t + e + 1
    | `Product fs -> fs |> List.fold_left (fun s (_, e) -> s + e) 1
    | `Mu e | `Lam (_, e) | `Inject (_, e) -> e + 1
    | `Select (e, l) -> e + l + 1
    | `Case cs -> cs + 1

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

  let dummy_var = `Var (Var.fresh Loc.dummy)

  let lam fn =
    let i = Var.fresh Loc.dummy in
    `Lam (i, fn @@ `Var i)

  let unapp t =
    let rec loop xs = function
      | `App (f, x) -> loop (x :: xs) f
      | f -> (f, xs)
    in
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
    | `Case _ as e ->
      let i = Var.fresh Loc.dummy in
      (is @ [i], `App (e, `Var i))
    | _ -> (is, e)

  let lams is = List.fold_right (fun i e -> `Lam (i, e)) is

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

  let rec called_at_tail n f' e =
    match unapp e with
    | `Var i, xs -> Var.equal i f' && List.length xs = n
    | `App _, _ -> failwith "called_at_tail"
    | `IfElse (c, t, e), [] -> called_at_tail n f' t || called_at_tail n f' e
    | `Lam (i, e), [_] -> (not (Var.equal i f')) && called_at_tail n f' e
    | `Case (`Product fs), [_] ->
      let v = `Var (Var.fresh Loc.dummy) in
      fs |> List.exists (fun (_, f) -> called_at_tail n f' (`App (f, v)))
    | `Mu e, [] -> false
    | _ -> false

  let rec is_total e =
    match e with
    | `Const _ | `Var _ | `Lam _ -> return true
    | _ ->
      let* seen = get Seen.field in
      if Seen.mem e seen then
        return false
      else
        Seen.adding e
          (match unapp e with
          | (`Const _ | `Var _ | `Lam _), [] -> return true
          | `IfElse (c, t, e), xs ->
            is_total c &&& is_total (apps t xs) &&& is_total (apps e xs)
          | `Product fs, _ -> fs |> List.for_all_fr (fun (_, e) -> is_total e)
          | `Mu (`Lam (i, e)), xs -> is_total (apps e xs) |> VarMap.adding i e
          | `Select (e, l), [] -> is_total e &&& is_total l
          | `Inject (_, e), _ -> is_total e
          | `Var f, xs -> (
            let* f_opt = VarMap.find_opt f in
            match f_opt with
            | None -> return false
            | Some f -> is_total (apps f xs))
          | `Lam (i, e), x :: xs ->
            is_total x
            &&& (is_total e |> VarMap.adding i x)
            &&& (is_total (apps e xs) |> VarMap.adding i x)
          | `Const c, xs ->
            return (Const.is_total c) &&& (xs |> List.for_all_fr is_total)
          | `Case (`Product fs), x :: xs ->
            is_total x
            &&& (fs
                |> List.for_all_fr (fun (_, f) ->
                       is_total (apps f (dummy_var :: xs))))
          | `Case e, [] -> is_total e
          | (`Mu _ | `App (_, _) | `Select _ | `Case _), _ -> return false)

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

  let rec is_lam_or_case = function
    | `Lam _ -> true
    | `Case (`Product fs) -> List.for_all (snd >>> is_lam_or_case) fs
    | _ -> false

  let rec occurs_once_in_total_position i' e =
    match unapp e with
    | `Var i, [] -> return @@ Var.equal i' i
    | `Const c, xs when Const.is_total c ->
      occurs_once_in_total_position_of_list i' xs
    | f, xs when is_lam_or_case f && not (is_free i' f) ->
      occurs_once_in_total_position_of_list i' xs
    | _ -> return false

  and occurs_once_in_total_position_of_list i' = function
    | [] -> return false
    | x :: xs ->
      occurs_once_in_total_position i' x
      &&& return (List.for_all (is_free i' >>> not) xs)
      ||| (return (not (is_free i' x))
          &&& is_total x
          &&& occurs_once_in_total_position_of_list i' xs)

  let is_mu = function `Mu _ -> true | _ -> false

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
      get Limit.field
      >>= Option.iter_fr (fun limit ->
              if limit < size e then fail `Limit else unit)
      >> Seen.adding e (simplify_base e >>- keep_phys_eq' e)

  and simplify_base = function
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
          let* limit = get Limit.field in
          let e = subst i x e in
          match limit with
          | None ->
            let new_limit = max (size e * 2) (size defaulted * 2) in
            setting Limit.field (Some new_limit) (simplify e)
          | Some _ -> simplify e
        in
        let* may_subst =
          is_total x
          &&& return ((not (is_mu x)) || not (is_free i e))
          ||| occurs_once_in_total_position i e
        in
        if may_subst then
          apply ()
          |> try_in
               (fun applied ->
                 if
                   size applied * 3 < size defaulted * 4
                   && Erased.compare applied defaulted <> 0
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
          if Erased.compare t e = 0 then
            is_total c >>- function
            | true -> t
            | _ -> `App (`Lam (Var.fresh Loc.dummy, t), c)
          else if Erased.compare c t = 0 then
            is_total c >>= function
            | true -> simplify @@ `App (`App (`Const `OpLogicalOr, c), e)
            | false -> default ()
          else if Erased.compare c e = 0 then
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

  let simplify e =
    simplify e |> try_in return @@ fun (`Limit | `Seen) -> return e

  let rec simplify_to_fixed_point e =
    let* e' = simplify e in
    if e == e' then
      return e'
    else
      simplify_to_fixed_point e'

  module ErasedMap = Map.Make (Erased)

  let move_constants_to_top inn =
    let cs = ref ErasedMap.empty in
    let add c =
      match ErasedMap.find_opt c !cs with
      | None ->
        let i = Var.fresh Loc.dummy in
        cs := ErasedMap.add c (i, ErasedMap.cardinal !cs, ref 1) !cs;
        `Var i
      | Some (i, _, n) ->
        n := !n + 1;
        `Var i
    in
    let rec analyze ~skip =
      let consider ?(skip = skip) vs r =
        match r with
        | _ when skip -> (vs, r)
        | `Const (`Keep _ | `LitBool _ | `LitNat _) | `Product [] | `Var _ ->
          (vs, r)
        | _ -> (vs, if VarSet.is_empty vs then add r else r)
      in
      function
      | `Const _ as r -> r |> consider VarSet.empty
      | `Var i as r -> r |> consider @@ VarSet.singleton i
      | `App (`Lam (i, e), x) ->
        let evs, e = analyze ~skip:false e and xvs, x = analyze ~skip:true x in
        `App (`Lam (i, e), x)
        |> consider ~skip:true @@ VarSet.union (VarSet.remove i evs) xvs
      | `App (`App (`Const c, x), y) when Const.is_bop c && Const.is_total c ->
        let xvs, x = analyze ~skip:false x and yvs, y = analyze ~skip:false y in
        `App (`App (`Const c, x), y) |> consider @@ VarSet.union xvs yvs
      | `App (`Const c, x) when Const.is_uop c && Const.is_total c ->
        let vs, x = analyze ~skip:false x in
        `App (`Const c, x) |> consider vs
      | `App (f, x) ->
        let fvs, f = analyze ~skip:false f and xvs, x = analyze ~skip:false x in
        `App (f, x) |> consider @@ VarSet.union fvs xvs
      | `IfElse (c, t, e) ->
        let cvs, c = analyze ~skip:false c
        and tvs, t = analyze ~skip:false t
        and evs, e = analyze ~skip:false e in
        `IfElse (c, t, e) |> consider @@ VarSet.union cvs (VarSet.union tvs evs)
      | `Product fs ->
        let vs, fs = analyze_product ~skip:false fs in
        `Product fs |> consider vs
      | `Mu (`Lam (f, e)) ->
        let vs, e = analyze ~skip:true e in
        `Mu (`Lam (f, e)) |> consider @@ VarSet.remove f vs
      | `Mu (`Case (`Product fs)) ->
        let vs, fs = analyze_product ~skip:true fs in
        `Mu (`Case (`Product fs)) |> consider vs
      | `Mu e ->
        let vs, e = analyze ~skip:false e in
        `Mu e |> consider vs
      | `Lam (i, e) ->
        let vs, e = analyze ~skip:false e in
        `Lam (i, e) |> consider @@ VarSet.remove i vs
      | `Inject (l, e) ->
        let vs, e = analyze ~skip:false e in
        `Inject (l, e) |> consider vs
      | `Select (e, `Inject (l, `Product [])) ->
        let vs, e = analyze ~skip:false e in
        `Select (e, `Inject (l, `Product [])) |> consider vs
      | `Select (e, l) ->
        let evs, e = analyze ~skip:false e and lvs, l = analyze ~skip:false l in
        `Select (e, l) |> consider @@ VarSet.union evs lvs
      | `Case (`Product fs) ->
        let vs, fs = analyze_product ~skip:true fs in
        `Case (`Product fs) |> consider vs
      | `Case cs ->
        let vs, cs = analyze ~skip:false cs in
        `Case cs |> consider vs
    and analyze_product ~skip fs =
      let fs = fs |> Row.map (analyze ~skip) in
      ( fs
        |> List.fold_left (fun s (_, (vs, _)) -> VarSet.union s vs) VarSet.empty,
        fs |> Row.map snd )
    in
    let _, e = analyze ~skip:true inn in
    !cs |> ErasedMap.bindings
    |> List.sort (fun (_, (_, l, _)) (_, (_, r, _)) -> Int.compare r l)
    |> List.fold_left (fun e (v, (i, _, _)) -> `App (`Lam (i, e), v)) e

  let is_const = function `Const _ -> true | _ -> false

  let to_return = function
    | `Return | `Tail _ -> str "return "
    | `Top -> str " "
    | `Const _ -> failwith "bug"

  let to_assignments is vs =
    match (is, vs) with
    | i :: is, v :: vs ->
      List.fold_left2
        (fun e i v -> e ^ str ", " ^ Var.to_js i ^ str " = " ^ v)
        (Var.to_js i ^ str " = " ^ v)
        is vs
    | _ -> failwith "to_assignments"

  let rec lam_bind_to_js_expr f b =
    let is, b' = unlam b |> and_uncase in
    if called_at_tail (List.length is) f b' then
      let i's = List.map Var.freshen is in
      let+ b = to_js_stmts (`Tail (f, i's)) VarSet.empty b' in
      let b =
        str "{for (;;) {const "
        ^ to_assignments is (List.map Var.to_js i's)
        ^ str ";" ^ b ^ str "}}"
      in
      List.fold_right (fun i b -> Var.to_js i ^ str " => " ^ b) i's b
    else
      to_js_expr b

  and to_js_stmts finish ids exp =
    let default () =
      match exp with
      | `Product [] -> (
        match finish with
        | `Top | `Return | `Tail _ -> return @@ str ""
        | `Const i -> return @@ str "const " ^ Var.to_js i ^ str " = void 0;")
      | exp -> (
        let+ e = to_js_expr exp in
        match finish with
        | `Const i -> str "const " ^ Var.to_js i ^ str " = " ^ e ^ str ";"
        | _ -> to_return finish ^ e ^ str ";")
    in
    match (unapp exp, finish) with
    | (`Var i, xs), `Tail (i', is)
      when Var.equal i i' && List.length xs = List.length is ->
      let+ vs = List.map_fr to_js_expr xs in
      to_assignments is vs ^ str "; continue"
    | _ -> (
      match exp with
      | `App (`Lam (i, e), v) -> (
        if VarSet.mem i ids then
          let i' = Var.freshen i in
          let vi' = `Var i' in
          to_js_stmts finish ids @@ `App (`Lam (i', subst i vi' e), v)
        else
          let body v =
            let b =
              if is_free i e then
                str "const " ^ Var.to_js i ^ str " = "
              else
                str ""
            in
            let+ e = to_js_stmts finish (VarSet.add i ids) e in
            b ^ v ^ str "; " ^ e
          in
          match v with
          | `Mu (`Lam (f, (`Product fs as b)))
            when fs |> List.for_all (snd >>> is_lam_or_case)
                 && always_selected f b
                 && (Var.equal i f || always_selected f e) ->
            let is =
              fs
              |> List.map @@ fun (l, _) ->
                 let i = Var.of_label l in
                 if VarSet.mem i ids || is_free i exp then Var.freshen i else i
            in
            let fs' =
              `Product (List.map2 (fun (l, _) i -> (l, `Var i)) fs is)
            in
            let* e =
              simplify (subst i fs' e)
              >>= to_js_stmts finish (VarSet.union ids (VarSet.of_list is))
            in
            let+ bs =
              List.fold_left2_fr
                (fun b i (_, v) ->
                  let* v = simplify (subst f fs' v) in
                  lam_bind_to_js_expr i v >>- fun v ->
                  b ^ str "const " ^ Var.to_js i ^ str " = " ^ v ^ str ";")
                (str "") is fs
            in
            bs ^ e
          | `Mu (`Lam (f, (`Lam _ as b)))
            when (not (is_free i v)) && is_free i e ->
            lam_bind_to_js_expr i (subst f (`Var i) b) >>= body
          | `Mu (`Lam (f, b))
            when (not (is_immediately_evaluated f b))
                 && (not (is_free i v))
                 && is_free i e ->
            to_js_expr (subst f (`Var i) b) >>= body
          | _ -> to_js_expr v >>= body)
      | `IfElse (c, t, e) when not (is_const finish) ->
        let+ c = to_js_expr c
        and+ t = to_js_stmts finish VarSet.empty t
        and+ e = to_js_stmts finish VarSet.empty e in
        str "if (" ^ c ^ str ") {" ^ t ^ str "} else {" ^ e ^ str "}"
      | `App (`Case (`Product fs), x) when not (is_const finish) ->
        let i0 = Var.fresh Loc.dummy in
        let i1 = Var.fresh Loc.dummy in
        let v1 = `Var i1 in
        let fs =
          fs
          |> List.fold_left
               (fun cs (l, e) ->
                 cs
                 |> ErasedMap.update e @@ function
                    | None -> Some [l]
                    | Some ls -> Some (l :: ls))
               ErasedMap.empty
        in
        let+ x = to_js_expr x
        and+ fs =
          fs |> ErasedMap.bindings
          |> List.sort
               (Compare.the (snd >>> List.length >>> ( ~- )) Int.compare)
          |> (function
               | (e, _) :: cs -> List.rev_append cs [(e, [])] | [] -> [])
          |> List.map_fr @@ fun (e, ls) ->
             let* e = simplify @@ `App (e, v1) in
             let+ e = to_js_stmts finish VarSet.empty e in
             let cs =
               match ls with
               | [] -> str "default: {"
               | ls ->
                 ls
                 |> List.fold_left
                      (fun s l ->
                        str "case " ^ Label.to_js_atom l ^ str ": " ^ s)
                      (str " {")
             in
             cs ^ e ^ str "}"
        in
        str "const [" ^ Var.to_js i0 ^ str ", " ^ Var.to_js i1 ^ str "] = " ^ x
        ^ str "; switch (" ^ Var.to_js i0 ^ str ") "
        ^ List.fold_left (fun es e -> es ^ e ^ str "; ") (str "{") fs
        ^ str "}"
      | `App (`Case cs, x) when not (is_const finish) ->
        let i = Var.fresh Loc.dummy in
        let+ x = to_js_expr x and+ cs = to_js_expr cs in
        str "const " ^ Var.to_js i ^ str " = " ^ x ^ str "; " ^ to_return finish
        ^ cs ^ str "[" ^ Var.to_js i ^ str "[0]](" ^ Var.to_js i ^ str "[1])"
      | `Mu (`Lam (f, e))
        when (not (is_const finish)) && not (is_immediately_evaluated f e) ->
        let+ e = to_js_expr e in
        str "const " ^ Var.to_js f ^ str " = " ^ e ^ str ";" ^ to_return finish
        ^ Var.to_js f
      | _ -> default ())

  and to_js_expr exp =
    match exp with
    | `Const c -> (
      match Const.type_of Loc.dummy c |> Typ.arity_and_result with
      | 2, result when Const.is_bop c ->
        return @@ parens @@ str "$1 => $2 => "
        ^ coerce_to_int_if (Typ.is_int result)
            (str "$1 " ^ Const.to_js c ^ str " $2")
      | 1, result when Const.is_uop c ->
        return @@ parens @@ str "$ => "
        ^ coerce_to_int_if (Typ.is_int result) (Const.to_js c ^ str " $")
      | _, _ -> return @@ Const.to_js c)
    | `Var i -> return @@ Var.to_js i
    | `Lam (i, `Mu (`Var i')) when Var.equal i i' -> return @@ str "rec"
    | `Lam (i, e) ->
      let+ e = to_js_stmts `Return (VarSet.singleton i) e in
      parens @@ Var.to_js i ^ str " => {" ^ e ^ str "}"
    | `Mu (`Lam (f, `Lam (x, e))) ->
      let+ e = to_js_stmts `Return (VarSet.singleton x) e in
      parens @@ str "function " ^ Var.to_js f ^ str "(" ^ Var.to_js x
      ^ str ") {" ^ e ^ str "}"
    | `Mu (`Lam (f, (`Case _ as e))) ->
      let x = Var.fresh Loc.dummy in
      let+ e = to_js_stmts `Return (VarSet.singleton x) @@ `App (e, `Var x) in
      parens @@ str "function " ^ Var.to_js f ^ str "(" ^ Var.to_js x
      ^ str ") {" ^ e ^ str "}"
    | `Mu (`Lam (f, e)) when not (is_immediately_evaluated f e) ->
      let+ e = to_js_expr e in
      str "(() => {const " ^ Var.to_js f ^ str " = " ^ e ^ str "; return "
      ^ Var.to_js f ^ str "})()"
    | `Mu f ->
      let+ f = to_js_expr f in
      str "rec(" ^ f ^ str ")"
    | `IfElse (c, t, e) ->
      let+ c = to_js_expr c and+ t = to_js_expr t and+ e = to_js_expr e in
      parens @@ c ^ str " ? " ^ t ^ str " : " ^ e
    | `Product [] -> return @@ str "void 0"
    | `Product fs ->
      let+ fs =
        fs
        |> List.map_fr @@ function
           | l, `Var i
             when Label.to_string l = Var.to_string i
                  && not (Js.is_illegal_id (Var.to_string i)) ->
             return @@ Label.to_js_label l
           | l, e ->
             let+ e = to_js_expr e in
             Label.to_js_label l ^ str ": " ^ e
      in
      parens
      @@ (fs |> List.fold_left (fun es e -> es ^ e ^ str ", ") (str "{"))
      ^ str "}"
    | `Select (e, `Inject (l, _)) ->
      let+ e = to_js_expr e in
      e ^ Label.to_js_select l
    | `Select (e, l) ->
      let+ e = to_js_expr e and+ l = to_js_expr l in
      e ^ str "[" ^ l ^ str "[0]]"
    | `Inject (l, `Product []) ->
      return @@ str "[" ^ Label.to_js_atom l ^ str "]"
    | `Inject (l, e) ->
      let+ e = to_js_expr e in
      str "[" ^ Label.to_js_atom l ^ str ", " ^ e ^ str "]"
    | `App (`Lam (i, e), v) ->
      let+ v = to_js_expr v
      and+ e = to_js_stmts `Return (VarSet.singleton i) e in
      str "(" ^ Var.to_js i ^ str " => {" ^ e ^ str "})(" ^ v ^ str ")"
    | `App (`Case _, _) as e ->
      let+ e = to_js_stmts `Return VarSet.empty e in
      str "(() => {" ^ e ^ str "})()"
    | `Case cs ->
      let i = Var.fresh Loc.dummy in
      to_js_expr @@ `Lam (i, `App (`Case cs, `Var i))
    | `App (f, x) -> (
      let default () =
        let+ f = to_js_expr f and+ x = to_js_expr x in
        f ^ str "(" ^ x ^ str ")"
      in
      match exp with
      | `App (`App (`Const c, lhs), rhs) when Const.is_bop c ->
        let n, result = Const.type_of Loc.dummy c |> Typ.arity_and_result in
        if n <> 2 then
          default ()
        else
          let+ lhs = to_js_expr lhs and+ rhs = to_js_expr rhs in
          parens
          @@ coerce_to_int_if (Typ.is_int result)
          @@ lhs ^ str " " ^ Const.to_js c ^ str " " ^ rhs
      | `App (`Const c, lhs) when Const.is_bop c ->
        let n, result = Const.type_of Loc.dummy c |> Typ.arity_and_result in
        let* lhs_is_total = is_total lhs in
        if (not lhs_is_total) || n <> 2 then
          default ()
        else
          let+ lhs = to_js_expr lhs in
          let rhs = Var.fresh Loc.dummy in
          parens @@ Var.to_js rhs ^ str " => "
          ^ coerce_to_int_if (Typ.is_int result)
          @@ lhs ^ str " " ^ Const.to_js c ^ str " " ^ Var.to_js rhs
      | `App (`Const c, rhs) when Const.is_uop c ->
        let n, result = Const.type_of Loc.dummy c |> Typ.arity_and_result in
        if n <> 1 then
          default ()
        else
          let+ rhs = to_js_expr rhs in
          parens @@ coerce_to_int_if (Typ.is_int result) (Const.to_js c ^ rhs)
      | _ -> default ())
end

let in_env () =
  with_env @@ fun _ ->
  object
    inherit Exp.VarMap.con
    inherit Exp.Limit.con
    inherit Exp.Seen.con
  end

let erase = Exp.erase

let simplify exp =
  let+ exp = exp |> Exp.simplify_to_fixed_point |> in_env () in
  Exp.move_constants_to_top exp

let to_js ?(top = `Top) exp =
  Exp.to_js_stmts top Exp.VarSet.empty exp |> in_env ()
