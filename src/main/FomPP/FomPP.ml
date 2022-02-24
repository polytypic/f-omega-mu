open FomBasis
open FomPPrint
open FomSource
open FomParser

module Kind = struct
  module UnkMap = FomAST.Kind.UnkMap

  module Numbering = struct
    include FomAST.Kind.UnkMap

    type nonrec t = int t ref

    let create () = ref empty
  end

  let pp ?(numbering = Numbering.create ()) kind =
    let rec pp atomize kind =
      match kind with
      | `Star _ -> star
      | `Arrow (_, dom, cod) ->
        let dom = pp true dom in
        let cod = pp false cod in
        dom ^^ space_arrow_right_break_1 ^^ cod
        |> if atomize then egyptian parens 2 else id
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
    pp false kind |> group

  let pp_annot ?(numbering = Numbering.create ()) = function
    | `Star _ -> empty
    | kind -> colon ^^ align (pp ~numbering kind)

  let to_string ?(numbering = Numbering.create ()) k =
    pp ~numbering k |> to_string
end

module Label = struct
  include FomAST.Label

  let pp l =
    let s = to_string l in
    if Lexer.is_id_or_nat s then utf8string s
    else s |> JsonString.of_utf8 |> JsonString.to_utf8_json |> utf8string
end

module Row = FomAST.Row

module Typ = struct
  module Const = struct
    let pp = function
      | `Bool -> bool'
      | `Int -> int'
      | `String -> string'
      | `Unit -> unit'
  end

  module Var = FomAST.Typ.Var

  (* *)

  let prec_min = 0
  let prec_arrow = 1
  let prec_join = 2
  let prec_meet = 3
  let prec_app = 4
  let prec_max = 5

  (* *)

  let some_spaces = Some spaces

  type ('t, 'k) config = {
    hr : bool;
    pp_annot : 'k -> document;
    pp : ('t, 'k) config -> int -> 't -> document;
  }

  let rec hanging = function
    | `Lam _ | `Mu (_, `Lam _) | `ForAll (_, `Lam _) | `Exists (_, `Lam _) ->
      some_spaces
    | `Product _ -> some_spaces
    | `App _ as t -> (
      match FomAST.Typ.unapp t with `Var _, [x] -> hanging x | _ -> None)
    | _ -> None

  let binding config prec_outer head i k t =
    (group (head ^^ Var.pp i ^^ config.pp_annot k ^^ dot |> nest 2)
    ^^
    match hanging t with
    | Some _ -> config.pp config prec_min t
    | None -> gnest 2 (break_0 ^^ group (config.pp config prec_min t)))
    |> if prec_min < prec_outer then egyptian parens 2 else id

  let quantifier config prec_outer symbol typ =
    match typ with
    | `Lam (_, id, kind, body) -> binding config prec_outer symbol id kind body
    | _ -> symbol ^^ egyptian parens 2 (config.pp config prec_min typ)

  let labeled config labels =
    labels
    |> List.stable_sort (Compare.the (fst >>> Label.at >>> fst) Pos.compare)
    |> List.map (function
         | l, `Var (_, i) when Var.name i = Label.name l -> Label.pp l
         | label, typ -> (
           Label.pp label ^^ colon
           ^^
           match hanging typ with
           | Some (lhs, _) -> lhs ^^ config.pp config prec_min typ
           | None -> gnest 2 (break_1 ^^ config.pp config prec_min typ)))
    |> separate comma_break_1_or_break_0

  let ticked config labels =
    match
      labels
      |> List.stable_sort (Compare.the (fst >>> Label.at >>> fst) Pos.compare)
      |> List.map @@ function
         | l, `Const (_, `Unit) -> tick ^^ Label.pp l
         | l, t ->
           gnest 2 (tick ^^ Label.pp l ^^ break_1 ^^ config.pp config prec_max t)
    with
    | [l] -> l
    | [] -> pipe
    | ls ->
      ls |> separate break_1_pipe_space |> precede (ifflat empty pipe_space)

  let tupled config labels =
    labels
    |> List.map (snd >>> config.pp config prec_min)
    |> separate comma_break_1

  let infix config prec_outer prec op l r =
    config.pp config prec l ^^ space ^^ op ^^ space ^^ config.pp config prec r
    |> if prec < prec_outer then egyptian parens 2 else id

  let rec as_aggr typ =
    match typ with
    | `Sum (_, [(l, t)]) ->
      let l = Label.to_string l in
      if l = FomCST.Exp.cons then
        match t with
        | `Product (_, ([(_, x); (_, xs)] as ls)) when Row.is_tuple ls ->
          as_aggr xs >>- fun xs -> x :: xs
        | _ -> zero
      else if l = FomCST.Exp.nil then
        match t with `Const (_, `Unit) -> return [] | _ -> zero
      else zero
    | _ -> zero

  let pp config prec_outer typ =
    match typ with
    | `Const (_, const) -> Const.pp const
    | `Var (_, id) -> Var.pp ~hr:config.hr id
    | `Lam (_, id, kind, body) ->
      binding config prec_outer lambda_lower id kind body
    | `Mu (_, typ) -> quantifier config prec_outer mu_lower typ
    | `ForAll (_, typ) -> quantifier config prec_outer FomPPrint.for_all typ
    | `Exists (_, typ) -> quantifier config prec_outer FomPPrint.exists typ
    | `Arrow (_, dom, cod) ->
      config.pp config (prec_arrow + 1) dom
      ^^ (match hanging cod with
         | Some (lhs, _) -> space_arrow_right ^^ lhs
         | None -> space_arrow_right_break_1)
      ^^ config.pp config (prec_arrow - 1) cod
      |> if prec_arrow < prec_outer then egyptian parens 2 else id
    | `Product (_, labels) ->
      if Row.is_tuple labels then tupled config labels |> egyptian parens 2
      else labeled config labels |> egyptian braces 2
    | `Sum _ as typ -> (
      match as_aggr typ |> Option.run with
      | Some xs ->
        xs
        |> List.map (config.pp config prec_min)
        |> separate comma_break_1 |> egyptian brackets 2
      | None -> (
        match typ with
        | `Sum (_, [(l, `Const (_, `Unit))]) -> tick ^^ Label.pp l
        | `Sum (_, labels) ->
          ticked config labels
          |> if prec_arrow < prec_outer then egyptian parens 2 else id))
    | `App (_, _, _) -> (
      match FomAST.Typ.unapp typ with
      | f, xs ->
        config.pp config prec_app f
        :: (xs |> List.map (config.pp config (prec_app + 1) >>> group))
        |> separate break_1
        |> if prec_app < prec_outer then egyptian parens 2 else group)
    | `Join (_, l, r) -> infix config prec_outer prec_join logical_or l r
    | `Meet (_, l, r) -> infix config prec_outer prec_meet logical_and l r

  let pp ?(hr = true)
      ?(pp_annot = Kind.pp_annot ~numbering:(Kind.Numbering.create ())) typ =
    pp {hr; pp_annot; pp} prec_min typ |> group

  let to_string t = t |> pp |> to_string
end

module Exp = struct
  module Const = struct
    let pp' nat typ = function
      | `Bool bool -> if bool then true' else false'
      | `Nat i -> nat i
      | `String s -> utf8string @@ JsonString.to_utf8_json s
      | `Unit -> unit'
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
        ^^ egyptian double_angle_quotes 2 (typ t)
        ^^ space ^^ utf8string @@ JsonString.to_utf8_json l

    let pp c = pp' (Bigint.to_string >>> utf8string) Typ.pp c
  end

  module Var = FomAST.Exp.Var
end
