open FomBasis
open FomSource
open FomAST
open Cats

(* *)

open Lam

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

module Var = struct
  include Var

  let to_js id =
    let id = to_string id in
    if Js.is_illegal_id id then str "$" ^ str id ^ str "$" else str id
end

module Const = struct
  include Const

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
end

let coerce_to_int exp = str "(" ^ exp ^ str ") | 0"
let coerce_to_int_if bool exp = if bool then coerce_to_int exp else exp
let parens exp = str "(" ^ exp ^ str ")"

let to_return = function
  | `Return | `Tail _ -> str "return "
  | `Top -> str " "
  | `Const _ -> failwith "bug"

let to_case = function
  | `Tail (f, is, i's, `Exit) -> `Tail (f, is, i's, `Case)
  | other -> other

let rec to_assignments is i's vs =
  List.fold_left3_fr
    (fun e i i' v ->
      match v with
      | `Var j' when Var.equal i j' -> return e
      | _ ->
        let+ v = to_js_expr v in
        (if e = str "" then e else e ^ str ", ") ^ Var.to_js i' ^ str " = " ^ v)
    (str "") is i's vs

and lam_bind_to_js_expr f b =
  let is, b' = unlam b |> and_uncase in
  if called_at_tail (List.length is) f b' then
    let i's = List.map Var.freshen is in
    let+ b = to_js_stmts (`Tail (f, is, i's, `Exit)) VarSet.empty b'
    and+ inits = to_assignments is is (List.map (fun i' -> `Var i') i's) in
    let b = str "{for (;;) {const " ^ inits ^ str ";" ^ b ^ str "}}" in
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
  | (`Var i, xs), `Tail (i', is, i's, ft)
    when Var.equal i i' && List.length xs = List.length i's -> (
    let+ assignments = to_assignments is i's xs in
    match ft with
    | `Case -> assignments ^ str "; continue"
    | `Exit -> assignments)
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
          let fs' = `Product (List.map2 (fun (l, _) i -> (l, `Var i)) fs is) in
          let* e =
            LamSimplify.once (subst i fs' e)
            >>= to_js_stmts finish (VarSet.union ids (VarSet.of_list is))
          in
          let+ bs =
            List.fold_left2_fr
              (fun b i (_, v) ->
                let* v = LamSimplify.once (subst f fs' v) in
                lam_bind_to_js_expr i v >>- fun v ->
                b ^ str "const " ^ Var.to_js i ^ str " = " ^ v ^ str ";")
              (str "") is fs
          in
          bs ^ e
        | `Mu (`Lam (f, (`Lam _ as b))) when (not (is_free i v)) && is_free i e
          ->
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
    | `App (`Case (`Product fs), x) when not (is_const finish) -> (
      let i0 = Var.fresh Loc.dummy in
      let i1 = Var.fresh Loc.dummy in
      let v1 = `Var i1 in
      let fs =
        fs
        |> List.fold_left
             (fun cs (l, e) ->
               cs
               |> LamMap.update e @@ function
                  | None -> Some [l]
                  | Some ls -> Some (l :: ls))
             LamMap.empty
      in
      let* decon =
        let+ x = to_js_expr x in
        str "const [" ^ Var.to_js i0 ^ str ", " ^ Var.to_js i1 ^ str "] = " ^ x
        ^ str ";"
      in
      match
        fs |> LamMap.bindings
        |> List.sort (Compare.the (snd >>> List.length >>> ( ~- )) Int.compare)
        |> function
        | (e, _) :: cs -> List.rev_append cs [(e, [])]
        | [] -> []
      with
      | [(t, _)] ->
        LamSimplify.once
          (`App
            ( t,
              `Select (x, `Inject (Label.of_string Loc.dummy "1", `Product []))
            ))
        >>= to_js_stmts finish VarSet.empty
      | [(t, [l]); (e, [])] ->
        let+ t =
          LamSimplify.once (`App (t, v1)) >>= to_js_stmts finish VarSet.empty
        and+ e =
          LamSimplify.once (`App (e, v1)) >>= to_js_stmts finish VarSet.empty
        in
        decon ^ str "if (" ^ Var.to_js i0 ^ str " === " ^ Label.to_js_atom l
        ^ str ") {" ^ t ^ str "} else {" ^ e ^ str "}"
      | cs ->
        let+ fs =
          cs
          |> List.map_fr @@ fun (e, ls) ->
             let* e = LamSimplify.once @@ `App (e, v1) in
             let+ e = to_js_stmts (to_case finish) VarSet.empty e in
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
        decon ^ str "switch (" ^ Var.to_js i0 ^ str ") "
        ^ List.fold_left (fun es e -> es ^ e ^ str "; ") (str "{") fs
        ^ str "}")
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
    parens @@ str "function " ^ Var.to_js f ^ str "(" ^ Var.to_js x ^ str ") {"
    ^ e ^ str "}"
  | `Mu (`Lam (f, (`Case _ as e))) ->
    let x = Var.fresh Loc.dummy in
    let+ e = to_js_stmts `Return (VarSet.singleton x) @@ `App (e, `Var x) in
    parens @@ str "function " ^ Var.to_js f ^ str "(" ^ Var.to_js x ^ str ") {"
    ^ e ^ str "}"
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
  | `Inject (l, `Product []) -> return @@ str "[" ^ Label.to_js_atom l ^ str "]"
  | `Inject (l, e) ->
    let+ e = to_js_expr e in
    str "[" ^ Label.to_js_atom l ^ str ", " ^ e ^ str "]"
  | `App (`Lam (i, e), v) ->
    let+ v = to_js_expr v and+ e = to_js_stmts `Return (VarSet.singleton i) e in
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
