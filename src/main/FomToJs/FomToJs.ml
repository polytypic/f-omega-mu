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

  let to_js (label : t) = str label.it
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
      | `LitNat nat ->
        let open Bigint in
        let nat = bit_and nat bi_2_pow_32_minus_1 in
        (* TODO: Warn when literal is truncated. *)
        if nat < bi_2_pow_31 then
          nat |> to_string |> str
        else
          str "(" ^ (nat - bi_2_pow_32 |> to_string |> str) ^ str ")"
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
  end

  module Id = struct
    include Id

    let to_js ({it; _} : t) =
      if Js.is_illegal_id it then
        str it ^ str "$"
      else
        str it
  end

  let coerce_to_int exp = str "(" ^ exp ^ str ") | 0"
  let coerce_to_int_if bool exp = if bool then coerce_to_int exp else exp
  let parens exp = str "(" ^ exp ^ str ")"
  let parens_if bool exp = if bool then parens exp else exp
  let braces exp = str "{" ^ exp ^ str "}"
  let braces_if bool exp = if bool then braces exp else exp

  module Ids = Set.Make (Id)

  let rec erase it =
    match it with
    | `Const (_, c) -> `Const c
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
    | `Case (_, e, cs) -> `Case (erase e, erase cs)
    | `Gen (_, _, _, e) | `Inst (_, e, _) | `Pack (_, _, e, _) -> erase e
    | `Target (_, _, s) -> `Target s

  let rec is_free i = function
    | `Const _ | `Target _ -> false
    | `Var i' -> Id.equal i' i
    | `Lam (i', e) -> (not (Id.equal i' i)) && is_free i e
    | `App (f, x) -> is_free i f || is_free i x
    | `IfElse (c, t, e) -> is_free i c || is_free i t || is_free i e
    | `Product fs -> fs |> List.exists (fun (_, e) -> is_free i e)
    | `Mu e | `Select (e, _) | `Inject (_, e) -> is_free i e
    | `Case (e, cs) -> is_free i e || is_free i cs

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

  let rec is_total e =
    match e with
    | `Const _ | `Var _ | `Lam _ -> true
    | `Mu (`Lam (_, e)) -> is_total e
    | `IfElse (c, t, e) -> is_total c && is_total t && is_total e
    | `Product fs -> fs |> List.for_all (fun (_, e) -> is_total e)
    | `Select (e, _) -> is_total e
    | `Inject (_, e) -> is_total e
    | `App (`Lam (_, e), x) -> is_total e && is_total x
    | `App (`Const _, x) -> is_total x
    | `App (`App (`Const _, x), y) -> is_total x && is_total y
    | `Case (_, _) | `Mu _ | `Target _ | `App (_, _) -> false

  module LabelMap = Map.Make (Label)

  let rec inline_select i m e =
    let ( let* ) = Option.bind in
    let traverse f xs =
      let rec loop ys = function
        | [] -> Some (List.rev ys)
        | x :: xs ->
          let* y = f x in
          loop (y :: ys) xs
      in
      loop [] xs
    in
    match e with
    | `Select (`Var i', l) when Id.equal i i' -> Some (`Var (LabelMap.find l m))
    | `Var i' -> if Id.equal i i' then None else Some e
    | `Const _ | `Target _ -> Some e
    | `Lam (i', b) ->
      if Id.equal i i' then
        Some e
      else
        let* b = inline_select i m b in
        Some (`Lam (i', b))
    | `App (f, x) ->
      let* f = inline_select i m f in
      let* x = inline_select i m x in
      Some (`App (f, x))
    | `Mu e ->
      let* e = inline_select i m e in
      Some (`Mu e)
    | `IfElse (c, t, e) ->
      let* c = inline_select i m c in
      let* t = inline_select i m t in
      let* e = inline_select i m e in
      Some (`IfElse (c, t, e))
    | `Product fs ->
      let* fs =
        fs
        |> traverse (fun (l, e) ->
               let* e = inline_select i m e in
               Some (l, e))
      in
      Some (`Product fs)
    | `Select (e, l) ->
      let* e = inline_select i m e in
      Some (`Select (e, l))
    | `Inject (l, e) ->
      let* e = inline_select i m e in
      Some (`Inject (l, e))
    | `Case (e, cs) ->
      let* e = inline_select i m e in
      let* cs = inline_select i m cs in
      Some (`Case (e, cs))

  let rec simplify e =
    match e with
    | `Const _ | `Target _ -> e
    | `Var _ -> e
    | `Lam (i, e) -> (
      match simplify e with
      | `App (f, `Var i')
        when Id.equal i i' && (not (is_free i f)) && is_total f ->
        f
      | e -> `Lam (i, e))
    | `App (f, x) -> (
      match (simplify f, simplify x) with
      | (`Lam (i, e) as f), (`Product fs as x) -> (
        let m =
          fs |> List.to_seq
          |> Seq.map (fun (l, _) ->
                 let i = Id.freshen {Id.it = l.Label.it; at = l.at} in
                 (l, i))
          |> LabelMap.of_seq
        in
        match inline_select i m e with
        | None -> `App (f, x)
        | Some e ->
          fs |> List.rev
          |> List.fold_left
               (fun b (l, e) ->
                 let i = LabelMap.find l m in
                 `App (`Lam (i, b), e))
               e
          |> simplify)
      | `Lam (i, e), `Var _ -> simplify (subst i x e)
      | `Lam (i, e), `App (`Lam (j, f), y) ->
        let j', f' =
          if is_free j e || Id.equal i j then
            let j' = Id.freshen j in
            let vj' = `Var j' in
            (j', subst j vj' f)
          else
            (j, f)
        in
        simplify (`App (`Lam (j', `App (`Lam (i, e), f')), y))
      | `Lam (i, e), x when is_total x && not (is_free i e) -> e
      | `Lam (i, `Var i'), x when Id.equal i i' -> x
      | f, x -> `App (f, x))
    | `Mu e -> `Mu (simplify e)
    | `IfElse (c, t, e) -> (
      match simplify c with
      | `Const (`LitBool c) -> simplify (if c then t else e)
      | _ -> `IfElse (c, simplify t, simplify e))
    | `Product fs -> `Product (fs |> List.map (fun (l, e) -> (l, simplify e)))
    | `Select (e, l) -> `Select (simplify e, l)
    | `Inject (l, e) -> `Inject (l, simplify e)
    | `Case (e, cs) -> `Case (simplify e, simplify cs)

  let binds = function `App (`Lam _, _) | `Case _ -> true | _ -> false

  let rec to_js_in_body ?(first = false) ?(block = false) ids exp =
    let open Reader in
    match exp with
    | `App (`Lam (i, e), v) ->
      if Ids.mem i ids then
        let i' = Id.freshen i in
        let vi' = `Var i' in
        to_js_in_body ~first ~block ids (`App (`Lam (i', subst i vi' e), v))
      else
        let* v =
          match v with
          | `Mu (`Lam (f, (`Lam (_, _) as l)))
            when Id.equal i f || not (is_free i l) ->
            to_js (subst f (`Var i) l)
          | _ -> to_js v
        in
        let* e = to_js_in_body (Ids.add i ids) e in
        return
          (braces_if first
             (str "const " ^ Id.to_js i ^ str " = " ^ v ^ str "; " ^ e))
    | `IfElse (c, t, e) ->
      if first && not (binds t || binds e) then
        to_js ~body:true exp
      else
        let* c = to_js c in
        let* t = to_js_in_body Ids.empty t in
        let* e = to_js_in_body Ids.empty e in
        return
          (braces_if first
             (str "if (" ^ c ^ str ") {" ^ t ^ str "} else {" ^ e ^ str "}"))
    | `Case (x, `Product fs) ->
      let i = Id.id Loc.dummy "$1" in
      let v = `Var i in
      let* x = to_js x in
      let* fs =
        fs
        |> traverse (fun (l, e) ->
               let* e =
                 to_js_in_body ~first:true ~block:true Ids.empty
                   (simplify (`App (e, v)))
               in
               return (str "case '" ^ Label.to_js l ^ str "': " ^ e))
      in
      return
        (braces_if first
           (str "const [$0, $1] = " ^ x ^ str "; switch ($0) "
           ^ List.fold_left (fun es e -> es ^ e ^ str "; ") (str "{") fs
           ^ str "}"))
    | `Case (x, cs) ->
      let* x = to_js x in
      let* cs = to_js ~atom:true cs in
      return
        (braces_if first
           (str "const $ = " ^ x ^ str "; return " ^ cs ^ str "[$[0]]($[1])"))
    | _ ->
      if first && not block then
        to_js ~body:true exp
      else
        let* e = to_js exp in
        return (str "return " ^ e)

  and to_js_wrap_as_body e =
    let open Reader in
    let* e = to_js_in_body ~first:true Ids.empty e in
    return (str "(() => " ^ e ^ str ")()")

  and to_js ?(body = false) ?(atom = false) exp =
    let open Reader in
    match exp with
    | `Const c -> (
      match Const.type_of Loc.dummy c |> Typ.arity_and_result with
      | 2, result ->
        return
          (parens_if atom
             (str "$1 => $2 => "
             ^ coerce_to_int_if (Typ.is_int result)
                 (str "$1 " ^ Const.to_js c ^ str " $2")))
      | 1, result ->
        return
          (parens_if atom
             (str "$ => "
             ^ coerce_to_int_if (Typ.is_int result) (Const.to_js c ^ str " $")))
      | 0, _ -> return (Const.to_js c)
      | n, _ -> failwithf "Unsupported arity %d" n)
    | `Var i -> return (Id.to_js i)
    | `Lam (i, `Mu (`Var i')) when Id.equal i i' -> return @@ str "rec"
    | `Lam (i, e) ->
      let* e = to_js_in_body ~first:true (Ids.singleton i) e in
      return (parens_if atom (Id.to_js i ^ str " => " ^ e))
    | `Mu (`Lam (f, `Lam (x, e))) ->
      let* e = to_js_in_body (Ids.singleton x) e in
      return
        (str "function " ^ Id.to_js f ^ str "(" ^ Id.to_js x ^ str ") {" ^ e
       ^ str "}")
    | `Mu f ->
      let* f = to_js f in
      return (str "rec(" ^ f ^ str ")")
    | `IfElse (c, t, e) ->
      let* c = to_js c in
      let* t = to_js t in
      let* e = to_js e in
      return (parens_if atom (c ^ str " ? " ^ t ^ str " : " ^ e))
    | `Product fs ->
      let* fs =
        fs
        |> traverse (function
             | l, `Var i
               when l.Label.it = i.Id.it && not (Js.is_illegal_id i.it) ->
               return (Label.to_js l)
             | l, e ->
               let* e = to_js e in
               return (Label.to_js l ^ str ": " ^ e))
      in
      return
        (parens_if (body || atom)
           ((fs |> List.fold_left (fun es e -> es ^ e ^ str ", ") (str "{"))
           ^ str "}"))
    | `Select (e, l) ->
      let* e = to_js ~atom:true e in
      return (e ^ str "." ^ Label.to_js l)
    | `Inject (l, e) ->
      let* e = to_js e in
      return
        (parens_if atom (str "[\"" ^ Label.to_js l ^ str "\", " ^ e ^ str "]"))
    | `App (`Lam (_, _), _) | `Case _ -> to_js_wrap_as_body exp
    | `App (f, x) -> (
      let default () =
        let* f = to_js ~atom:true f in
        let* x = to_js x in
        return (f ^ str "(" ^ x ^ str ")")
      in
      match exp with
      | `App (`App (`Const c, lhs), rhs) ->
        let n, result = Const.type_of Loc.dummy c |> Typ.arity_and_result in
        if n <> 2 then
          default ()
        else
          let* lhs = to_js ~atom:true lhs in
          let* rhs = to_js ~atom:true rhs in
          return
            (parens_if atom
               (coerce_to_int_if (Typ.is_int result)
                  (lhs ^ str " " ^ Const.to_js c ^ str " " ^ rhs)))
      | `App (`Const c, rhs) ->
        let n, result = Const.type_of Loc.dummy c |> Typ.arity_and_result in
        if n <> 1 then
          default ()
        else
          let* rhs = to_js ~atom:true rhs in
          return
            (parens_if atom
               (coerce_to_int_if (Typ.is_int result) (Const.to_js c ^ rhs)))
      | _ -> default ())
    | `Target lit ->
      let buffer = Buffer.create (String.length lit * 2) in
      let encoder = Uutf.encoder `UTF_8 (`Buffer buffer) in
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
               Uutf.encode encoder (`Uchar c) |> ignore;
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
      return (parens_if (body || atom) (str (Buffer.contents buffer)))
end

let to_js exp =
  exp |> Exp.erase |> Exp.simplify |> Exp.to_js ~body:true |> Reader.run ()
  |> to_string
