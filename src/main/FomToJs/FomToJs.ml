open FomSource
open FomBasis
open FomAST

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

  let to_str = to_string >> str

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
      | `LitString lit -> str lit
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

  let coerce_to_int exp = str "(" ^ exp ^ str ") | 0"
  let coerce_to_int_if bool exp = if bool then coerce_to_int exp else exp
  let parens exp = str "(" ^ exp ^ str ")"
  let parens_if bool exp = if bool then parens exp else exp
  let braces exp = str "{" ^ exp ^ str "}"
  let braces_if bool exp = if bool then braces exp else exp

  module Env = Map.Make (Id)
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
    | `Product (_, fs) -> `Product (fs |> List.map (fun (l, e) -> (l, erase e)))
    | `Select (_, e, l) -> `Select (erase e, l)
    | `Inject (_, l, e, _) -> `Inject (l, erase e)
    | `Case (_, s, cs) -> `Case (erase s, erase cs)
    | `Gen (_, _, _, e) | `Inst (_, e, _) | `Pack (_, _, e, _) -> erase e
    | `Target (_, _, s) -> `Target s

  let rec bottomUp fn = function
    | (`Target _ | `Const _ | `Var _) as e -> fn e
    | `App (f, x) -> fn (`App (bottomUp fn f, bottomUp fn x))
    | `IfElse (c, t, e) ->
      fn (`IfElse (bottomUp fn c, bottomUp fn t, bottomUp fn e))
    | `Product fs ->
      fn (`Product (fs |> List.map (fun (l, e) -> (l, bottomUp fn e))))
    | `Mu e -> fn (`Mu (bottomUp fn e))
    | `Lam (i, e) -> fn (`Lam (i, bottomUp fn e))
    | `Inject (l, e) -> fn (`Inject (l, bottomUp fn e))
    | `Select (e, l) -> fn (`Select (bottomUp fn e, l))
    | `Case (s, cs) -> fn (`Case (bottomUp fn s, bottomUp fn cs))

  let size =
    bottomUp @@ function
    | `Target _ | `Const _ | `Var _ -> 1
    | `App (f, x) -> f + x + 1
    | `IfElse (c, t, e) -> c + t + e + 1
    | `Product fs -> fs |> List.fold_left (fun s (_, e) -> s + e) 1
    | `Mu e | `Lam (_, e) | `Inject (_, e) | `Select (e, _) -> e + 1
    | `Case (s, cs) -> s + cs + 1

  (*
  let free =
    bottomUp @@ function
    | `Var i -> Ids.singleton i
    | `Lam (i, e) -> Ids.remove i e
    | `Target _ | `Const _ -> Ids.empty
    | `App (f, x) -> Ids.union f x
    | `IfElse (c, t, e) -> Ids.union c (Ids.union t e)
    | `Product fs ->
      fs |> List.fold_left (fun s (_, e) -> Ids.union s e) Ids.empty
    | `Mu e | `Inject (_, e) | `Select (e, _) -> e
    | `Case (s, cs) -> Ids.union s cs
*)
  let rec is_free i' = function
    | `Const _ | `Target _ -> false
    | `Var i -> Id.equal i' i
    | `Lam (i, e) -> (not (Id.equal i' i)) && is_free i' e
    | `App (f, x) -> is_free i' f || is_free i' x
    | `IfElse (c, t, e) -> is_free i' c || is_free i' t || is_free i' e
    | `Product fs -> fs |> List.exists (snd >> is_free i')
    | `Mu e | `Select (e, _) | `Inject (_, e) -> is_free i' e
    | `Case (s, cs) -> is_free i' s || is_free i' cs

  let rec subst i the inn =
    match inn with
    | `Const _ | `Target _ -> inn
    | `Var i' -> if Id.equal i' i then the else inn
    | `Lam (i', e) ->
      if Id.equal i' i then
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
    | `Product fs -> `Product (fs |> List.map (fun (l, e) -> (l, subst i the e)))
    | `Select (e, l) -> `Select (subst i the e, l)
    | `Inject (l, e) -> `Inject (l, subst i the e)
    | `Case (e, cs) -> `Case (subst i the e, subst i the cs)

  let dummy_var = `Var (Id.fresh Loc.dummy)

  let lam fn =
    let i = Id.fresh Loc.dummy in
    `Lam (i, fn @@ `Var i)

  let rec is_total =
    let open Reader in
    function
    | `Const _ | `Var _ | `Lam _ -> return true
    | `IfElse (c, t, e) -> is_total c &&& is_total t &&& is_total e
    | `Product fs -> fs |> for_all (fun (_, e) -> is_total e)
    | `Mu (`Lam (i, e)) -> is_total e << Env.add i e
    | `Select (e, _) | `Inject (_, e) -> is_total e
    | `App (`Var f, x) -> (
      let* f_opt r = Env.find_opt f r in
      match f_opt with None -> return false | Some f -> is_total (`App (f, x)))
    | `App (`Lam (i, e), x) -> is_total x &&& (is_total e << Env.add i x)
    | `App (`Const _, x) -> is_total x
    | `App (`App (`Const _, x), y) -> is_total x &&& is_total y
    | `Case (s, `Product fs) ->
      is_total s
      &&& (fs |> for_all (fun (_, f) -> is_total (`App (f, dummy_var))))
    | `Case (_, _) | `Mu _ | `Target _ | `App (_, _) -> return false

  let is_lam = function `Lam _ -> true | _ -> false

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

  let may_inline_continuation = function
    | `IfElse _ | `App (`Lam _, _) -> true
    | `Case (_, `Product fs) when List.for_all (snd >> is_lam) fs -> true
    | _ -> false

  let rec inline_continuation e k =
    match e with
    | `IfElse (c, t, e) ->
      `IfElse (c, inline_continuation t k, inline_continuation e k)
    | `Case (s, `Product fs) when List.for_all (snd >> is_lam) fs ->
      let fs =
        fs
        |> List.map (function
             | l, `Lam (i, e) -> (l, to_lam inline_continuation k i e)
             | _ -> failwith "impossible")
      in
      `Case (s, `Product fs)
    | `App (`Lam (i, e), v) -> `App (to_lam inline_continuation k i e, v)
    | ( `Const _ | `Var _ | `Lam _ | `Mu _ | `Product _ | `Select _ | `Inject _
      | `App _ | `Case _ | `Target _ ) as e ->
      `App (k, e)

  (*
  let may_hoist_let = function `App (`Lam _, _) -> true | _ -> false

  let rec hoist_let e k =
    match e with
    | `App (`Lam (i, e), x) -> `App (to_lam hoist_let k i e, x)
    | e -> `App (k, e)
  *)

  let rec simplify =
    let open Reader in
    function
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
        if f_is_total then
          return f
        else
          default ()
      | _ -> default ())
    | `App (f, x) -> (
      let* f = simplify f in
      let* x = simplify x in
      let* f =
        match f with
        | `Lam (i, e) ->
          let* e = simplify e << Env.add i x in
          return @@ `Lam (i, e)
        | _ -> return f
      in
      let default () = return @@ `App (f, x) in
      match (f, x) with
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
        let apply () = simplify (subst i x e) in
        let* x_is_total = is_total x in
        if x_is_total then
          let* applied = apply () in
          let* defaulted = default () in
          return
            (if size applied * 3 < size defaulted * 4 then
               applied
            else
              defaulted)
        else
          default ()
      | _ -> default ())
    | `Mu e -> (
      let* e = simplify e in
      match e with
      | `Lam (i, e) when not (is_free i e) -> return e
      | e -> return @@ `Mu e)
    | `IfElse (c, t, e) -> (
      let* c = simplify c in
      match c with
      | `Const (`LitBool c) -> simplify (if c then t else e)
      | _ ->
        let* t = simplify t in
        let* e = simplify e in
        return @@ `IfElse (c, t, e))
    | `Product fs ->
      let* fs =
        fs
        |> traverse (fun (l, e) ->
               let* e = simplify e in
               return (l, e))
      in
      return @@ `Product fs
    | `Select (e, l) -> (
      let* e = simplify e in
      let default () = return @@ `Select (e, l) in
      match e with
      | `Product fs ->
        let* fs_are_total =
          fs
          |> List.filter (fst >> Label.equal l >> not)
          |> for_all (fun (_, e) -> is_total e)
        in
        if fs_are_total then
          return @@ (fs |> List.find (fst >> Label.equal l) |> snd)
        else
          default ()
      | _ -> default ())
    | `Inject (l, e) ->
      let* e = simplify e in
      return @@ `Inject (l, e)
    | `Case (s, cs) ->
      let* s = simplify s in
      let* cs = simplify cs in
      let default () = return @@ `Case (s, cs) in
      let* cs_is_total = is_total cs in
      if cs_is_total then
        match (s, cs) with
        | `Inject (l, e), `Product fs ->
          simplify @@ `App (List.find (fst >> Label.equal l) fs |> snd, e)
        | _, `Product _ when may_inline_continuation s ->
          let* inlined =
            simplify (inline_continuation s (lam @@ fun s -> `Case (s, cs)))
          in
          let* defaulted = default () in
          return
            (if size inlined * 3 < size defaulted * 4 then
               inlined
            else
              defaulted)
        | _ -> default ()
      else
        default ()

  let binds = function `App (`Lam _, _) | `Case _ -> true | _ -> false

  let rec to_js_stmts ?(add_braces = false) ?(add_return = false) ids exp =
    let open Reader in
    let default () =
      if add_return then
        let* e = to_js_expr exp in
        return @@ str "return " ^ e
      else
        to_js_expr ~body:true exp
    in
    match exp with
    | `App (`Lam (i, e), v) ->
      if Ids.mem i ids then
        let i' = Id.freshen i in
        let vi' = `Var i' in
        to_js_stmts ~add_braces ~add_return ids
        @@ `App (`Lam (i', subst i vi' e), v)
      else
        let* v =
          match v with
          | `Mu (`Lam (f, (`Lam (_, _) as l)))
            when Id.equal i f || not (is_free i l) ->
            to_js_expr (subst f (`Var i) l)
          | _ -> to_js_expr v
        in
        let* e =
          to_js_stmts ~add_return:(add_braces || add_return) (Ids.add i ids) e
        in
        return @@ braces_if add_braces @@ str "const " ^ Id.to_js i ^ str " = "
        ^ v ^ str "; " ^ e
    | `IfElse (c, t, e) ->
      if add_braces && not (binds t || binds e) then
        default ()
      else
        let* c = to_js_expr c in
        let add_return = add_braces || add_return in
        let* t = to_js_stmts ~add_return Ids.empty t in
        let* e = to_js_stmts ~add_return Ids.empty e in
        return @@ braces_if add_braces @@ str "if (" ^ c ^ str ") {" ^ t
        ^ str "} else {" ^ e ^ str "}"
    | `Case (x, `Product fs) when add_braces || add_return ->
      let i0 = Id.fresh Loc.dummy in
      let i1 = Id.fresh Loc.dummy in
      let v1 = `Var i1 in
      let* x = to_js_expr x in
      let* fs =
        fs
        |> traverse (fun (l, e) ->
               let* e = simplify @@ `App (e, v1) in
               let* e =
                 to_js_stmts ~add_braces:true ~add_return:true Ids.empty e
               in
               return @@ str "case " ^ Label.to_js_atom l ^ str ": " ^ e)
      in
      return @@ braces_if add_braces @@ str "const [" ^ Id.to_js i0 ^ str ", "
      ^ Id.to_js i1 ^ str "] = " ^ x ^ str "; switch (" ^ Id.to_js i0 ^ str ") "
      ^ List.fold_left (fun es e -> es ^ e ^ str "; ") (str "{") fs
      ^ str "}"
    | `Case (x, cs) when add_braces || add_return ->
      let i = Id.fresh Loc.dummy in
      let* x = to_js_expr x in
      let* cs = to_js_expr ~atom:true cs in
      return @@ braces_if add_braces @@ str "const " ^ Id.to_js i ^ str " = "
      ^ x ^ str "; return " ^ cs ^ str "[" ^ Id.to_js i ^ str "[0]]("
      ^ Id.to_js i ^ str "[1])"
    | _ -> default ()

  and to_js_expr ?(body = false) ?(atom = false) exp =
    let open Reader in
    match exp with
    | `Const c -> (
      match Const.type_of Loc.dummy c |> Typ.arity_and_result with
      | 2, result ->
        return @@ parens_if atom @@ str "$1 => $2 => "
        ^ coerce_to_int_if (Typ.is_int result)
            (str "$1 " ^ Const.to_js c ^ str " $2")
      | 1, result ->
        return @@ parens_if atom @@ str "$ => "
        ^ coerce_to_int_if (Typ.is_int result) (Const.to_js c ^ str " $")
      | 0, _ -> return @@ Const.to_js c
      | n, _ -> failwithf "Unsupported arity %d" n)
    | `Var i -> return @@ Id.to_js i
    | `Lam (i, `Mu (`Var i')) when Id.equal i i' -> return @@ str "rec"
    | `Lam (i, e) ->
      let* e = to_js_stmts ~add_braces:true (Ids.singleton i) e in
      return @@ parens_if atom @@ Id.to_js i ^ str " => " ^ e
    | `Mu (`Lam (f, `Lam (x, e))) ->
      let* e = to_js_stmts ~add_return:true (Ids.singleton x) e in
      return @@ parens_if atom @@ str "function " ^ Id.to_js f ^ str "("
      ^ Id.to_js x ^ str ") {" ^ e ^ str "}"
    | `Mu f ->
      let* f = to_js_expr f in
      return @@ str "rec(" ^ f ^ str ")"
    | `IfElse (c, t, e) ->
      let* c = to_js_expr c in
      let* t = to_js_expr t in
      let* e = to_js_expr e in
      return @@ parens_if atom @@ c ^ str " ? " ^ t ^ str " : " ^ e
    | `Product fs ->
      let* fs =
        fs
        |> traverse (function
             | l, `Var i
               when Label.to_string l = Id.to_string i
                    && not (Js.is_illegal_id (Id.to_string i)) ->
               return @@ Label.to_js_label l
             | l, e ->
               let* e = to_js_expr e in
               return @@ Label.to_js_label l ^ str ": " ^ e)
      in
      return
      @@ parens_if (body || atom)
      @@ (fs |> List.fold_left (fun es e -> es ^ e ^ str ", ") (str "{"))
      ^ str "}"
    | `Select (e, l) ->
      let* e = to_js_expr ~atom:true e in
      return (e ^ Label.to_js_select l)
    | `Inject (l, e) ->
      let* e = to_js_expr e in
      return @@ parens_if atom @@ str "[" ^ Label.to_js_atom l ^ str ", " ^ e
      ^ str "]"
    | `App (`Lam (i, e), v) ->
      let* v = to_js_expr v in
      let* e = to_js_stmts ~add_braces:true (Ids.singleton i) e in
      return @@ str "(" ^ Id.to_js i ^ str " => " ^ e ^ str ")(" ^ v ^ str ")"
    | `Case _ as e ->
      let* e = to_js_stmts ~add_return:true Ids.empty e in
      return @@ str "(() => {" ^ e ^ str "})()"
    | `App (f, x) -> (
      let default () =
        let* f = to_js_expr ~atom:true f in
        let* x = to_js_expr x in
        return @@ f ^ str "(" ^ x ^ str ")"
      in
      match exp with
      | `App (`App (`Const c, lhs), rhs) ->
        let n, result = Const.type_of Loc.dummy c |> Typ.arity_and_result in
        if n <> 2 then
          default ()
        else
          let* lhs = to_js_expr ~atom:true lhs in
          let* rhs = to_js_expr ~atom:true rhs in
          return @@ parens_if atom
          @@ coerce_to_int_if (Typ.is_int result)
          @@ lhs ^ str " " ^ Const.to_js c ^ str " " ^ rhs
      | `App (`Const c, rhs) ->
        let n, result = Const.type_of Loc.dummy c |> Typ.arity_and_result in
        if n <> 1 then
          default ()
        else
          let* rhs = to_js_expr ~atom:true rhs in
          return @@ parens_if atom
          @@ coerce_to_int_if (Typ.is_int result) (Const.to_js c ^ rhs)
      | _ -> default ())
    | `Target lit ->
      let buffer = Buffer.create (String.length lit * 2) in
      let encoder = Uutf.encoder `UTF_8 @@ `Buffer buffer in
      let hex_to_int h c =
        (h lsl 4)
        lor
        if Uchar.of_char '0' <= c && c <= Uchar.of_char '9' then
          Uchar.to_int c - Uchar.to_int (Uchar.of_char '0')
        else if Uchar.of_char 'a' <= c && c <= Uchar.of_char 'f' then
          Uchar.to_int c - Uchar.to_int (Uchar.of_char 'a') + 10
        else
          Uchar.to_int c - Uchar.to_int (Uchar.of_char 'A') + 10
      in
      lit
      |> Uutf.String.fold_utf_8
           (fun (s, i) _ u ->
             let encode c =
               Uutf.encode encoder @@ `Uchar c |> ignore;
               (`Unescaped, i + 1)
             in
             match (s, u) with
             | `Unescaped, `Uchar c ->
               if Uchar.of_char '\\' = c then
                 (`Escaped, i + 1)
               else if Uchar.of_char '"' = c then
                 (`Unescaped, i + 1)
               else
                 encode c
             | `Escaped, `Uchar c ->
               if
                 Uchar.of_char '"' = c
                 || Uchar.of_char '\\' = c
                 || Uchar.of_char '/' = c
               then
                 encode c
               else if Uchar.of_char 'b' = c then
                 encode (Uchar.of_char '\b')
               else if Uchar.of_char 'f' = c then
                 encode (Uchar.of_int 0x0c)
               else if Uchar.of_char 'n' = c then
                 encode (Uchar.of_char '\n')
               else if Uchar.of_char 'r' = c then
                 encode (Uchar.of_char '\r')
               else if Uchar.of_char 't' = c then
                 encode (Uchar.of_char '\t')
               else
                 (`Hex0, i + 1)
             | `Hex0, `Uchar c -> (`Hex1 (hex_to_int 0 c), i + 1)
             | `Hex1 h, `Uchar c -> (`Hex2 (hex_to_int h c), i + 1)
             | `Hex2 h, `Uchar c -> (`Hex3 (hex_to_int h c), i + 1)
             | `Hex3 h, `Uchar c -> encode (Uchar.of_int (hex_to_int h c))
             | _, `Malformed _ ->
               failwithf "Malformed UTF-8 in string literal at char index %d" i)
           (`Unescaped, 0)
      |> ignore;
      Uutf.encode encoder `End |> ignore;
      return @@ parens_if (body || atom) @@ str (Buffer.contents buffer)
end

let to_js exp =
  exp |> Exp.erase |> Exp.simplify |> Reader.run Exp.Env.empty
  |> Exp.to_js_stmts Exp.Ids.empty
  |> Reader.run Exp.Env.empty |> to_string
