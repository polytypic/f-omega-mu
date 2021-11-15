open FomBasis
open FomPP
open FomSource

(* *)

module Kind = FomAST.Kind
module Label = FomAST.Label
module Exp = FomAST.Exp

type t = Loc.t * document

let nested d = nest 2 (break_1_0 ^^ d) ^^ break_1_0

let cyclic kind new_at filename old_at =
  return
    ( ( new_at,
        text "The file"
        ^^ nested (utf8format "\"%s\"" filename)
        ^^ textf "is part of an %s cycle " kind ),
      [(old_at, textf "Start of cyclic %s chain" kind)] )

let of_error = function
  (* IO errors *)
  | `Error_io (at, exn) ->
    return ((at, exn |> Printexc.to_string |> utf8string), [])
  (* Syntax errors *)
  | `Error_lexeme (at, lexeme) | `Error_grammar (at, lexeme) -> (
    match lexeme with
    | "" -> return ((at, text "Syntax error "), [])
    | lexeme ->
      return ((at, text "Syntax error" ^^ nested (utf8string lexeme)), []))
  | `Error_duplicated_label (at, l) ->
    return
      ( (at, text "Duplicated label" ^^ nested (Label.pp l)),
        [(Label.at l, text "Initial"); (at, text "Duplicate")] )
  | `Error_duplicated_typ_bind (at, i) ->
    return
      ( (at, text "Duplicated type binding" ^^ nested (Typ.Var.pp i)),
        [(Typ.Var.at i, text "Initial"); (at, text "Duplicate")] )
  | `Error_duplicated_bind (at, i) ->
    return
      ( (at, text "Duplicated binding" ^^ nested (Exp.Var.pp i)),
        [(Exp.Var.at i, text "Initial"); (at, text "Duplicate")] )
  (* Source errors *)
  | `Error_file_doesnt_exist (at, filename) ->
    return
      ( ( at,
          text "File"
          ^^ nested (utf8format "\"%s\"" filename)
          ^^ text "doesn't exist " ),
        [(at, text "File doesn't exist")] )
  | `Error_cyclic_includes (new_at, filename, old_at) ->
    cyclic "include" new_at filename old_at
  | `Error_cyclic_imports (new_at, filename, old_at) ->
    cyclic "import" new_at filename old_at
  (* Kind errors *)
  | `Error_kind_mismatch (at, expected_kind, actual_kind) ->
    return
      ( ( at,
          text "Expected type to have kind"
          ^^ nested (Kind.pp expected_kind)
          ^^ text "but the type has kind"
          ^^ nested (Kind.pp actual_kind) ),
        [
          (at, text "Kind mismatch");
          (Kind.at expected_kind, text "Expected type");
        ] )
  | `Error_cyclic_kind at ->
    return ((at, text "Cyclic kind"), [(at, text "Cyclic kind")])
  | `Error_mu_nested (at, typ, arg) ->
    return
      ( ( at,
          text "Nested types like"
          ^^ nested (Typ.pp typ)
          ^^ text "are not allowed to keep type checking decidable " ),
        [(Typ.at arg, text "Nested argument passed to μ type constructor")] )
  | `Error_mu_non_contractive (at, typ, arg) ->
    return
      ( ( at,
          text "Non-contractive types like"
          ^^ nested (Typ.pp typ)
          ^^ text "are not allowed " ),
        [(Typ.at arg, text "Non-contractive apply of μ type constructor")] )
  | `Error_typ_var_unbound (at, id) ->
    return
      ( (at, text "Unbound type variable" ^^ nested (Typ.Var.pp id)),
        [(Typ.Var.at id, text "Unbound type variable")] )
  (* Type errors *)
  | `Error_var_unbound (at, id) ->
    return
      ( (at, text "Unbound variable" ^^ nested (Exp.Var.pp id)),
        [(Exp.Var.at id, FomPP.text "Unbound variable")] )
  | `Error_typ_mismatch (at, expected_typ, actual_typ) ->
    return
      ( ( at,
          text "Expected expression to have type"
          ^^ nested (Typ.pp expected_typ)
          ^^ text "but the expression has type"
          ^^ nested (Typ.pp actual_typ) ),
        [
          (at, text "Type mismatch"); (Typ.at expected_typ, text "Expected type");
        ] )
  | `Error_typ_unexpected (at, mnemo, typ) ->
    let+ typ = Typ.contract typ in
    ( ( at,
        textf "Expected a %s type but the expression has type" mnemo
        ^^ nested (Typ.pp typ) ),
      [(at, textf "Expected a %s type" mnemo)] )
  | `Error_product_lacks (at, typ, label) ->
    let+ typ = Typ.contract typ in
    ( ( at,
        text "Expected expression to have a type of the form"
        ^^ nested (text "{" ^^ Label.pp label ^^ text ": _, _}")
        ^^ text "but the expression has type"
        ^^ nested (Typ.pp typ) ),
      [(at, text "Product lacks label")] )
  | `Error_label_missing (at, label, l_typ, m_typ) ->
    let+ l_typ = Typ.contract l_typ and+ m_typ = Typ.contract m_typ in
    ( ( at,
        text "Label"
        ^^ nested (Label.pp label)
        ^^ text "missing from type"
        ^^ nested (Typ.pp m_typ)
        ^^ text "to match the type"
        ^^ nested (Typ.pp l_typ) ),
      [(Typ.at m_typ, text "Label missing")] )
  | `Error_typ_var_escapes (at, i, t) ->
    let+ t = Typ.contract t in
    ( ( at,
        text "The ∃ type variable"
        ^^ nested (Typ.Var.pp i)
        ^^ text "escapes as part of the type"
        ^^ nested (Typ.pp t)
        ^^ text "of the expression " ),
      [(Typ.Var.at i, text "∃ type variable")] )
  | `Error_non_disjoint_merge (at, l, r) ->
    let+ l = Typ.contract l and+ r = Typ.contract r in
    ( ( at,
        text "Values of type"
        ^^ nested (Typ.pp l)
        ^^ text "and"
        ^^ nested (Typ.pp r)
        ^^ text "are not disjoint and cannot be merged " ),
      [(Typ.at l, text "Conflicting type"); (Typ.at r, text "Conflicting type")]
    )

let pp = function
  | (loc, overview), [] -> overview ^^ Loc.pp loc ^^ dot
  | (loc, overview), details ->
    overview ^^ Loc.pp loc ^^ dot ^^ break_0_0
    ^^ (details
       |> List.map (fun (loc, msg) ->
              text "Also" ^^ softbreak_1 ^^ Loc.pp loc
              ^^ gnest 2 (colon_break_1 ^^ msg ^^ dot))
       |> separate break_0_0)
