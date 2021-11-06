open FomBasis
open FomPP
open FomSource

(* *)

module Kind = FomAST.Kind
module Label = FomAST.Label
module Exp = FomAST.Exp

type t = Loc.t * document

let cyclic kind new_at filename old_at =
  return
    ( ( new_at,
        utf8string "The file"
        ^^ nest 2 (break_1 ^^ utf8format "\"%s\"" filename)
        ^^ break_1
        ^^ utf8format "is part of an %s cycle" kind ),
      [(old_at, utf8format "Start of cyclic %s chain" kind)] )

let of_error = function
  (* IO errors *)
  | `Error_io (at, exn) ->
    return ((at, exn |> Printexc.to_string |> utf8string), [])
  (* Syntax errors *)
  | `Error_lexeme (at, lexeme) | `Error_grammar (at, lexeme) -> (
    match lexeme with
    | "" -> return ((at, utf8string "Syntax error"), [])
    | lexeme -> return ((at, utf8format "Syntax error: %s" lexeme), []))
  | `Error_duplicated_label (at, l) ->
    return
      ( (at, utf8string "Duplicated label" ^^ nest 2 (break_1_0 ^^ Label.pp l)),
        [(Label.at l, utf8string "Initial"); (at, utf8string "Duplicate")] )
  | `Error_duplicated_typ_bind (at, i) ->
    return
      ( ( at,
          utf8string "Duplicated type binding"
          ^^ nest 2 (break_1_0 ^^ Typ.Var.pp i) ),
        [(Typ.Var.at i, utf8string "Initial"); (at, utf8string "Duplicate")] )
  | `Error_duplicated_bind (at, i) ->
    return
      ( ( at,
          utf8string "Duplicated binding" ^^ nest 2 (break_1_0 ^^ Exp.Var.pp i)
        ),
        [(Exp.Var.at i, utf8string "Initial"); (at, utf8string "Duplicate")] )
  (* Source errors *)
  | `Error_file_doesnt_exist (at, filename) ->
    return
      ( ( at,
          utf8string "File"
          ^^ nest 2 (break_1_0 ^^ utf8format "\"%s\"" filename)
          ^^ break_1_0 ^^ utf8string "doesn't exist" ),
        [(at, utf8format "File doesn't exist")] )
  | `Error_cyclic_includes (new_at, filename, old_at) ->
    cyclic "include" new_at filename old_at
  | `Error_cyclic_imports (new_at, filename, old_at) ->
    cyclic "import" new_at filename old_at
  (* Kind errors *)
  | `Error_kind_mismatch (at, expected_kind, actual_kind) ->
    return
      ( ( at,
          utf8string "Expected type to have kind"
          ^^ nest 2 (break_1_0 ^^ Kind.pp expected_kind)
          ^^ break_1_0
          ^^ utf8string "but the type has kind"
          ^^ nest 2 (break_1_0 ^^ Kind.pp actual_kind) ),
        [
          (at, utf8string "Kind mismatch");
          (Kind.at expected_kind, utf8string "Expected type");
        ] )
  | `Error_cyclic_kind at ->
    return ((at, utf8string "Cyclic kind"), [(at, utf8string "Cyclic kind")])
  | `Error_mu_nested (at, typ, arg) ->
    return
      ( ( at,
          utf8string "Nested types like"
          ^^ nest 2 (break_1_0 ^^ Typ.pp typ)
          ^^ break_1_0
          ^^ utf8string "are not allowed to keep type checking decidable" ),
        [
          (Typ.at arg, utf8string "Nested argument passed to μ type constructor");
        ] )
  | `Error_mu_non_contractive (at, typ, arg) ->
    return
      ( ( at,
          utf8string "Non-contractive types like"
          ^^ nest 2 (break_1_0 ^^ Typ.pp typ)
          ^^ break_1_0
          ^^ utf8string "are not allowed" ),
        [(Typ.at arg, utf8string "Non-contractive apply of μ type constructor")]
      )
  | `Error_typ_var_unbound (at, id) ->
    return
      ( (at, utf8string "Unbound type variable " ^^ Typ.Var.pp id),
        [(Typ.Var.at id, utf8string "Unbound type variable")] )
  (* Type errors *)
  | `Error_var_unbound (at, id) ->
    return
      ( (at, utf8string "Unbound variable " ^^ Exp.Var.pp id),
        [(Exp.Var.at id, FomPP.utf8string "Unbound variable")] )
  | `Error_typ_mismatch (at, expected_typ, actual_typ) ->
    return
      ( ( at,
          utf8string "Expected expression to have type"
          ^^ nest 2 (break_1_0 ^^ Typ.pp expected_typ)
          ^^ break_1_0
          ^^ utf8string "but the expression has type"
          ^^ nest 2 (break_1_0 ^^ Typ.pp actual_typ) ),
        [
          (at, utf8string "Type mismatch");
          (Typ.at expected_typ, utf8string "Expected type");
        ] )
  | `Error_typ_unexpected (at, mnemo, typ) ->
    let+ typ = Typ.contract typ in
    ( ( at,
        utf8format "Expected a %s type but the expression has type" mnemo
        ^^ nest 2 (break_1_0 ^^ Typ.pp typ) ),
      [(at, utf8format "Expected a %s type" mnemo)] )
  | `Error_product_lacks (at, typ, label) ->
    let+ typ = Typ.contract typ in
    ( ( at,
        utf8string "Expected expression to have a type of the form"
        ^^ nest 2
             (break_1_0 ^^ utf8string "{" ^^ Label.pp label
            ^^ utf8string ": _, _}")
        ^^ break_1_0
        ^^ utf8string "but the expression has type"
        ^^ nest 2 (break_1_0 ^^ Typ.pp typ) ),
      [(at, utf8string "Product lacks label")] )
  | `Error_label_missing (at, label, l_typ, m_typ) ->
    let+ l_typ = Typ.contract l_typ and+ r_typ = Typ.contract m_typ in
    ( ( at,
        utf8string "Label"
        ^^ nest 2 (break_1_0 ^^ Label.pp label)
        ^^ break_1_0
        ^^ utf8string "missing from type"
        ^^ nest 2 (break_1_0 ^^ Typ.pp m_typ)
        ^^ break_1_0
        ^^ utf8string "to match the type"
        ^^ nest 2 (break_1_0 ^^ Typ.pp l_typ) ),
      [(Typ.at m_typ, utf8string "Label missing")] )
  | `Error_typ_var_escapes (at, i, t) ->
    let+ t = Typ.contract t in
    ( ( at,
        utf8string "The ∃ type variable"
        ^^ nest 2 (break_1_0 ^^ Typ.Var.pp i)
        ^^ break_1_0
        ^^ utf8string "escapes as part of the type"
        ^^ nest 2 (break_1_0 ^^ Typ.pp t)
        ^^ break_1_0
        ^^ utf8string "of the expression" ),
      [(Typ.Var.at i, utf8string "∃ type variable")] )
  | `Error_non_disjoint_merge (at, l, r) ->
    let+ l = Typ.contract l and+ r = Typ.contract r in
    ( ( at,
        utf8string "Values of type"
        ^^ nest 2 (break_1_0 ^^ Typ.pp l)
        ^^ break_1_0 ^^ utf8string "and"
        ^^ nest 2 (break_1_0 ^^ Typ.pp r)
        ^^ break_1_0
        ^^ utf8string "are not disjoint and cannot be merged" ),
      [
        (Typ.at l, utf8string "Conflicting type");
        (Typ.at r, utf8string "Conflicting type");
      ] )

let pp = function
  | (loc, overview), [] -> gnest 2 (overview ^^ break_0_0 ^^ Loc.pp loc ^^ dot)
  | (loc, overview), details ->
    overview ^^ break_0_0 ^^ Loc.pp loc ^^ dot ^^ break_0_0
    ^^ (details
       |> List.map (fun (loc, msg) ->
              gnest 2
                (utf8string "Also " ^^ Loc.pp loc ^^ colon_break_1 ^^ msg ^^ dot))
       |> separate break_0_0)
