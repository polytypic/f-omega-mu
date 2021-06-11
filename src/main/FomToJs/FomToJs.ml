open FomPP
open FomBasis
open FomSource
open FomAST

(* *)

open Rea

type cstr = Str of string | Join of cstr * cstr

let str lit = Str lit
let ( ^ ) lhs rhs = Join (lhs, rhs)

let to_string cstr =
  let buffer = Buffer.create 1000 in
  let rec doit = function
    | Str str -> Buffer.add_string buffer str
    | Join (lhs, rhs) ->
      doit lhs;
      doit rhs
  in
  doit cstr;
  Buffer.contents buffer

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
      | `LitString lit -> str @@ LitString.to_utf8_json lit
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
        | `OpLogicalAnd | `OpLogicalNot | `OpLogicalOr ) as other ->
        other

    let is_commutative = function
      | `OpArithAdd | `OpArithMul | `OpEq _ | `OpEqNot _ -> true
      | `LitBool _ | `LitNat _ | `LitString _ | `OpArithDiv | `OpArithMinus
      | `OpArithPlus | `OpArithRem | `OpArithSub | `OpCmpGt | `OpCmpGtEq
      | `OpCmpLt | `OpCmpLtEq | `OpLogicalAnd | `OpLogicalNot | `OpLogicalOr ->
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
      | `OpArithAdd, `Var i, `Var j when Id.equal i j ->
        Some (`App (`App (`Const `OpArithMul, `Const (`LitNat 2l)), `Var i))
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
      | `OpArithSub, `Var i, `Var j when Id.equal i j ->
        Some (`Const (`LitNat 0l))
      | `OpArithSub, `Const (`LitNat x), `Const (`LitNat y) ->
        Some (`Const (`LitNat (Int32.sub x y)))
      | `OpArithSub, x, `Const (`LitNat 0l) -> Some x
      | `OpArithSub, `Const (`LitNat 0l), x ->
        Some (`App (`Const `OpArithMinus, x))
      | `OpArithSub, x, `App (`Const `OpArithMinus, y) ->
        Some (`App (`App (`Const `OpArithAdd, x), y))
      (* *)
      | _ -> None
  end

  module Id = struct
    include Id

    let to_js id =
      let id = to_string id in
      if Js.is_illegal_id id then
        str id ^ str "$"
      else
        str id
  end

  module Erased = struct
    type t =
      [ `App of t * t
      | `Case of t
      | `Const of (int32, Typ.t) Exp.Const.t
      | `IfElse of t * t * t
      | `Inject of Label.t * t
      | `Lam of Id.t * t
      | `Mu of t
      | `Product of (Label.t * t) list
      | `Select of t * Label.t
      | `Target of LitString.t
      | `Var of Id.t ]

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
      | `Target _ -> 9
      | `Var _ -> 10

    let rec compare l r =
      match (l, r) with
      | `App (fl, xl), `App (fr, xr) ->
        compare fl fr <>? fun () -> compare xl xr
      | `Case l, `Case r | `Mu l, `Mu r -> compare l r
      | `Const l, `Const r ->
        FomAST.Exp.Const.compare' Int32.compare Typ.compare l r
      | `IfElse (cl, tl, el), `IfElse (cr, tr, er) ->
        compare cl cr <>? fun () ->
        compare tl tr <>? fun () -> compare el er
      | `Select (el, ll), `Select (er, lr) | `Inject (ll, el), `Inject (lr, er)
        ->
        Label.compare ll lr <>? fun () -> compare el er
      | `Lam (vl, el), `Lam (vr, er) ->
        Id.compare vl vr <>? fun () -> compare el er
      | `Product lls, `Product rls ->
        ListExt.compare_with
          (fun (ll, el) (lr, er) ->
            Label.compare ll lr <>? fun () -> compare el er)
          lls rls
      | `Target l, `Target r -> LitString.compare l r
      | `Var l, `Var r -> Id.compare l r
      | _ -> index l - index r

    let[@warning "-32"] rec pp = function
      | `App (f, x) -> [pp f; space; pp x] |> concat |> egyptian parens 2
      | `Case t -> [utf8string "case"; space; pp t] |> concat
      | `Const c ->
        FomAST.Exp.Const.pp' (Int32.to_string >>> utf8string) FomAST.Typ.pp c
      | `IfElse (c, t, e) ->
        [
          [utf8string "if"; space; pp c; space] |> concat;
          [utf8string "then"; space; pp t; space] |> concat;
          [utf8string "else"; space; pp e; space] |> concat;
        ]
        |> concat |> egyptian parens 2
      | `Inject (l, t) ->
        [Label.pp l; equals; pp t] |> concat |> egyptian brackets 2
      | `Lam (v, t) ->
        [lambda_lower; Id.pp v; dot; pp t] |> concat |> egyptian parens 2
      | `Mu t -> [mu_lower; pp t |> egyptian parens 2] |> concat
      | `Product ls ->
        ls
        |> List.map (fun (l, t) -> [Label.pp l; equals; pp t] |> concat)
        |> separate comma_break_1 |> egyptian braces 2
      | `Select (t, l) -> [pp t; dot; Label.pp l] |> concat
      | `Target s ->
        [utf8string "target"; space; LitString.to_utf8_json s |> utf8string]
        |> concat |> egyptian parens 2
      | `Var v -> Id.pp v
  end

  let coerce_to_int exp = str "(" ^ exp ^ str ") | 0"
  let coerce_to_int_if bool exp = if bool then coerce_to_int exp else exp
  let parens exp = str "(" ^ exp ^ str ")"

  module Env = struct
    include Map.Make (Id)

    type nonrec t = Erased.t t

    let field r = r#env
    let find_opt i = get_as field (find_opt i)
    let adding i e = Rea.mapping field (add i e)

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

    class con =
      object
        val seen : t = empty
        method seen = Field.make seen (fun v -> {<seen = v>})
      end
  end

  module Ids = Set.Make (Id)

  let rec erase = function
    | `Const (_, c) -> `Const (Const.erase c)
    | `Var (_, i) -> `Var i
    | `Lam (_, i, _, e) -> `Lam (i, erase e)
    | `App (_, f, x) -> `App (erase f, erase x)
    | `UnpackIn (_, _, i, v, e) | `LetIn (_, i, v, e) ->
      `App (`Lam (i, erase e), erase v)
    | `Mu (_, e) -> `Mu (erase e)
    | `IfElse (_, c, t, e) -> `IfElse (erase c, erase t, erase e)
    | `Product (_, fs) -> `Product (fs |> List.map (Pair.map Fun.id erase))
    | `Select (_, e, l) -> `Select (erase e, l)
    | `Inject (_, l, e) -> `Inject (l, erase e)
    | `Case (_, cs) -> `Case (erase cs)
    | `Gen (_, _, _, e) | `Inst (_, e, _) | `Pack (_, _, e, _) -> erase e
    | `Target (_, _, s) -> `Target s

  let rec bottomUp fn = function
    | (`Target _ | `Const _ | `Var _) as e -> fn e
    | `App (f, x) -> fn (`App (bottomUp fn f, bottomUp fn x))
    | `IfElse (c, t, e) ->
      fn (`IfElse (bottomUp fn c, bottomUp fn t, bottomUp fn e))
    | `Product fs ->
      fn (`Product (fs |> List.map (Pair.map Fun.id (bottomUp fn))))
    | `Mu e -> fn (`Mu (bottomUp fn e))
    | `Lam (i, e) -> fn (`Lam (i, bottomUp fn e))
    | `Inject (l, e) -> fn (`Inject (l, bottomUp fn e))
    | `Select (e, l) -> fn (`Select (bottomUp fn e, l))
    | `Case cs -> fn (`Case (bottomUp fn cs))

  let size =
    bottomUp @@ function
    | `Target _ | `Const _ | `Var _ -> 1
    | `App (f, x) -> f + x + 1
    | `IfElse (c, t, e) -> c + t + e + 1
    | `Product fs -> fs |> List.fold_left (fun s (_, e) -> s + e) 1
    | `Mu e | `Lam (_, e) | `Inject (_, e) | `Select (e, _) -> e + 1
    | `Case cs -> cs + 1

  let rec is_free i' = function
    | `Const _ | `Target _ -> false
    | `Var i -> Id.equal i' i
    | `Lam (i, e) -> (not (Id.equal i' i)) && is_free i' e
    | `App (f, x) -> is_free i' f || is_free i' x
    | `IfElse (c, t, e) -> is_free i' c || is_free i' t || is_free i' e
    | `Product fs -> fs |> List.exists (snd >>> is_free i')
    | `Mu e | `Select (e, _) | `Inject (_, e) | `Case e -> is_free i' e

  let rec subst i the inn =
    match inn with
    | `Const _ | `Target _ -> inn
    | `Var i' -> if Id.equal i' i then the else inn
    | `Lam (i', e) ->
      if Id.equal i' i || not (is_free i e) then
        inn
      else if is_free i' the then
        let i'' = Id.freshen i' in
        let vi'' = `Var i'' in
        `Lam (i'', subst i the (subst i' vi'' e))
      else
        `Lam (i', subst i the e)
    | `App (f, x) -> `App (subst i the f, subst i the x)
    | `Mu e -> `Mu (subst i the e)
    | `IfElse (c, t, e) -> `IfElse (subst i the c, subst i the t, subst i the e)
    | `Product fs -> `Product (fs |> List.map (Pair.map Fun.id (subst i the)))
    | `Select (e, l) -> `Select (subst i the e, l)
    | `Inject (l, e) -> `Inject (l, subst i the e)
    | `Case cs -> `Case (subst i the cs)

  let dummy_var = `Var (Id.fresh Loc.dummy)

  let lam fn =
    let i = Id.fresh Loc.dummy in
    `Lam (i, fn @@ `Var i)

  let rec is_total e =
    let* seen = get Seen.field in
    if Seen.mem e seen then
      return false
    else
      mapping Seen.field (Seen.add e)
        (match e with
        | `Const _ | `Var _ | `Lam _ -> return true
        | `IfElse (c, t, e) -> is_total c &&& is_total t &&& is_total e
        | `Product fs -> fs |> MList.for_all (fun (_, e) -> is_total e)
        | `Mu (`Lam (i, e)) -> is_total e |> Env.adding i e
        | `Select (e, _) | `Inject (_, e) -> is_total e
        | `App (`Var f, x) -> (
          let* f_opt = Env.find_opt f in
          match f_opt with
          | None -> return false
          | Some f -> is_total (`App (f, x)))
        | `App (`Lam (i, e), x) -> is_total x &&& (is_total e |> Env.adding i x)
        | `App (`Const _, x) -> is_total x
        | `App (`App (`Const _, x), y) -> is_total x &&& is_total y
        | `App (`Case (`Product fs), s) ->
          is_total s
          &&& (fs
              |> MList.for_all (fun (_, f) -> is_total (`App (f, dummy_var))))
        | `Case e -> is_total e
        | `Mu _ | `Target _ | `App (_, _) -> return false)

  let rec is_lam_or_case = function
    | `Lam _ -> true
    | `Case (`Product fs) -> List.for_all (snd >>> is_lam_or_case) fs
    | _ -> false

  let to_lam continue k i e =
    let i, e =
      if is_free i k then
        let i' = Id.freshen i in
        let vi' = `Var i' in
        (i', subst i vi' e)
      else
        (i, e)
    in
    `Lam (i, continue e k)

  let rec to_case continue k fs =
    `Case
      (`Product
        (fs
        |> List.map
             (Pair.map Fun.id @@ function
              | `Lam (i, e) -> to_lam continue k i e
              | `Case (`Product fs) -> to_case continue k fs
              | _ -> failwith "impossible")))

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
      | _ -> failwith "impossible")
    | ( `Const _ | `Var _ | `Lam _ | `Mu _ | `Product _ | `Select _ | `Inject _
      | `App _ | `Case _ | `Target _ ) as e ->
      `App (k, e)

  let rec simplify e =
    let* seen = get Seen.field in
    if Seen.mem e seen then
      fail `Seen
    else
      get Limit.field
      >>= MOption.iter (fun limit ->
              if limit < size e then fail `Limit else unit)
      >> mapping Seen.field (Seen.add e) (simplify_base e)

  and simplify_base = function
    | (`Const _ | `Target _ | `Var _) as e -> return e
    | `Lam (i, `Lam (j, `App (`App (`Const c, `Var y), `Var x)))
      when Id.equal i x && Id.equal j y && Const.is_commutative c ->
      return @@ `Const c
    | `Lam (i, e) -> (
      let* e = simplify e in
      let default () = return @@ `Lam (i, e) in
      match e with
      | `App (f, `Var i') when Id.equal i i' && not (is_free i f) ->
        let* f_is_total = is_total f in
        if f_is_total then return f else default ()
      | _ -> default ())
    | `App (f, x) -> (
      let* f = simplify f and* x = simplify x in
      let* f =
        match f with
        | `Lam (i, e) ->
          let+ e = simplify e |> Env.adding i x in
          `Lam (i, e)
        | _ -> return f
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
      | `Const c, x -> (
        match Const.simplify_uop (c, x) with
        | Some e -> simplify e
        | None -> return @@ `App (`Const c, x))
      | `App (`Const c, x), y -> (
        match Const.simplify_bop (c, x, y) with
        | Some e -> simplify e
        | None -> return @@ `App (`App (`Const c, x), y))
      | `Lam (i, e), `App (`Lam (j, f), y) ->
        let j', f' =
          if is_free j e || Id.equal i j then
            let j' = Id.freshen j in
            let vj' = `Var j' in
            (j', subst j vj' f)
          else
            (j, f)
        in
        simplify @@ `App (`Lam (j', `App (`Lam (i, e), f')), y)
      | `Lam (i, `Var i'), x when Id.equal i i' -> return x
      | `Lam (i, e), x ->
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
        let* x_is_total = is_total x in
        if x_is_total then
          apply ()
          |> try_in
               (fun applied ->
                 return
                   (if size applied * 3 < size defaulted * 4 then
                      applied
                   else
                     defaulted))
               (fun (`Limit | `Seen) -> return defaulted)
        else
          return defaulted
      | _ -> default ())
    | `Mu e -> (
      let+ e = simplify e in
      match e with `Lam (i, e) when not (is_free i e) -> e | e -> `Mu e)
    | `IfElse (c, t, e) -> (
      let* c = simplify c in
      match c with
      | `Const (`LitBool c) -> simplify (if c then t else e)
      | _ ->
        let+ t = simplify t and+ e = simplify e in
        `IfElse (c, t, e))
    | `Product fs ->
      let+ fs = fs |> MList.traverse @@ MPair.traverse return simplify in
      `Product fs
    | `Select (e, l) -> (
      let* e = simplify e in
      let default () = return @@ `Select (e, l) in
      match e with
      | `Product fs ->
        let* fs_are_total =
          fs
          |> List.filter (fst >>> Label.equal l >>> not)
          |> MList.for_all (fun (_, e) -> is_total e)
        in
        if fs_are_total then
          return @@ (fs |> List.find (fst >>> Label.equal l) |> snd)
        else
          default ()
      | _ -> default ())
    | `Case cs ->
      let+ cs = simplify cs in
      `Case cs
    | `Inject (l, e) ->
      let+ e = simplify e in
      `Inject (l, e)

  let simplify e =
    simplify e |> try_in return @@ fun (`Limit | `Seen) -> return e

  let rec to_js_stmts is_top ids exp =
    let default () =
      let+ e = to_js_expr exp in
      (if is_top then str "" else str "return ") ^ e ^ str ";"
    in
    match exp with
    | `App (`Lam (i, e), v) ->
      if Ids.mem i ids then
        let i' = Id.freshen i in
        let vi' = `Var i' in
        to_js_stmts is_top ids @@ `App (`Lam (i', subst i vi' e), v)
      else
        let* v =
          match v with
          | `Mu (`Lam (f, (`Lam (_, _) as l)))
            when Id.equal i f || not (is_free i l) ->
            to_js_expr (subst f (`Var i) l)
          | _ -> to_js_expr v
        in
        let b =
          if is_free i e then str "const " ^ Id.to_js i ^ str " = " else str ""
        in
        let+ e = to_js_stmts is_top (Ids.add i ids) e in
        b ^ v ^ str "; " ^ e
    | `IfElse (c, t, e) ->
      let+ c = to_js_expr c
      and+ t = to_js_stmts is_top Ids.empty t
      and+ e = to_js_stmts is_top Ids.empty e in
      str "if (" ^ c ^ str ") {" ^ t ^ str "} else {" ^ e ^ str "}"
    | `App (`Case (`Product fs), x) ->
      let i0 = Id.fresh Loc.dummy in
      let i1 = Id.fresh Loc.dummy in
      let v1 = `Var i1 in
      let+ x = to_js_expr x
      and+ fs =
        fs
        |> MList.traverse @@ fun (l, e) ->
           let* e = simplify @@ `App (e, v1) in
           let+ e = to_js_stmts is_top Ids.empty e in
           str "case " ^ Label.to_js_atom l ^ str ": {" ^ e ^ str "}"
      in
      str "const [" ^ Id.to_js i0 ^ str ", " ^ Id.to_js i1 ^ str "] = " ^ x
      ^ str "; switch (" ^ Id.to_js i0 ^ str ") "
      ^ List.fold_left (fun es e -> es ^ e ^ str "; ") (str "{") fs
      ^ str "}"
    | `App (`Case cs, x) ->
      let i = Id.fresh Loc.dummy in
      let+ x = to_js_expr x and+ cs = to_js_expr cs in
      str "const " ^ Id.to_js i ^ str " = " ^ x ^ str "; "
      ^ (if is_top then str "" else str "return ")
      ^ cs ^ str "[" ^ Id.to_js i ^ str "[0]](" ^ Id.to_js i ^ str "[1])"
    | _ -> default ()

  and to_js_expr exp =
    match exp with
    | `Const c -> (
      match Const.type_of Loc.dummy c |> Typ.arity_and_result with
      | 2, result ->
        return @@ parens @@ str "$1 => $2 => "
        ^ coerce_to_int_if (Typ.is_int result)
            (str "$1 " ^ Const.to_js c ^ str " $2")
      | 1, result ->
        return @@ parens @@ str "$ => "
        ^ coerce_to_int_if (Typ.is_int result) (Const.to_js c ^ str " $")
      | 0, _ -> return @@ Const.to_js c
      | n, _ -> failwithf "Unsupported arity %d" n)
    | `Var i -> return @@ Id.to_js i
    | `Lam (i, `Mu (`Var i')) when Id.equal i i' -> return @@ str "rec"
    | `Lam (i, e) ->
      let+ e = to_js_stmts false (Ids.singleton i) e in
      parens @@ Id.to_js i ^ str " => {" ^ e ^ str "}"
    | `Mu (`Lam (f, `Lam (x, e))) ->
      let+ e = to_js_stmts false (Ids.singleton x) e in
      parens @@ str "function " ^ Id.to_js f ^ str "(" ^ Id.to_js x ^ str ") {"
      ^ e ^ str "}"
    | `Mu f ->
      let+ f = to_js_expr f in
      str "rec(" ^ f ^ str ")"
    | `IfElse (c, t, e) ->
      let+ c = to_js_expr c and+ t = to_js_expr t and+ e = to_js_expr e in
      parens @@ c ^ str " ? " ^ t ^ str " : " ^ e
    | `Product [] -> return @@ str "undefined"
    | `Product fs ->
      let+ fs =
        fs
        |> MList.traverse @@ function
           | l, `Var i
             when Label.to_string l = Id.to_string i
                  && not (Js.is_illegal_id (Id.to_string i)) ->
             return @@ Label.to_js_label l
           | l, e ->
             let+ e = to_js_expr e in
             Label.to_js_label l ^ str ": " ^ e
      in
      parens
      @@ (fs |> List.fold_left (fun es e -> es ^ e ^ str ", ") (str "{"))
      ^ str "}"
    | `Select (e, l) ->
      let+ e = to_js_expr e in
      e ^ Label.to_js_select l
    | `Inject (l, e) ->
      let+ e = to_js_expr e in
      str "[" ^ Label.to_js_atom l ^ str ", " ^ e ^ str "]"
    | `App (`Lam (i, e), v) ->
      let+ v = to_js_expr v and+ e = to_js_stmts false (Ids.singleton i) e in
      str "(" ^ Id.to_js i ^ str " => {" ^ e ^ str "})(" ^ v ^ str ")"
    | `App (`Case _, _) as e ->
      let+ e = to_js_stmts false Ids.empty e in
      str "(() => {" ^ e ^ str "})()"
    | `Case cs ->
      let i = Id.fresh Loc.dummy in
      to_js_expr @@ `Lam (i, `App (`Case cs, `Var i))
    | `App (f, x) -> (
      let default () =
        let+ f = to_js_expr f and+ x = to_js_expr x in
        f ^ str "(" ^ x ^ str ")"
      in
      match exp with
      | `App (`App (`Const c, lhs), rhs) ->
        let n, result = Const.type_of Loc.dummy c |> Typ.arity_and_result in
        if n <> 2 then
          default ()
        else
          let+ lhs = to_js_expr lhs and+ rhs = to_js_expr rhs in
          parens
          @@ coerce_to_int_if (Typ.is_int result)
          @@ lhs ^ str " " ^ Const.to_js c ^ str " " ^ rhs
      | `App (`Const c, rhs) ->
        let n, result = Const.type_of Loc.dummy c |> Typ.arity_and_result in
        if n <> 1 then
          default ()
        else
          let+ rhs = to_js_expr rhs in
          parens @@ coerce_to_int_if (Typ.is_int result) (Const.to_js c ^ rhs)
      | _ -> default ())
    | `Target lit -> return @@ parens @@ str @@ LitString.to_utf8 lit
end

let in_env () =
  with_env @@ fun _ ->
  object
    inherit Exp.Env.con
    inherit Exp.Limit.con
    inherit Exp.Seen.con
  end

let to_js exp =
  let exp = exp |> Exp.erase in
  let* exp = exp |> Exp.simplify |> in_env () in
  let+ js = Exp.to_js_stmts true Exp.Ids.empty exp |> in_env () in
  to_string js
