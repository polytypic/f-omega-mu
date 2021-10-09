open FomPP
open FomSource
open FomAST

type t = Loc.t * document

let cyclic kind new_at filename old_at =
  ( ( new_at,
      utf8string "The file"
      ^^ nest 2 (break_1 ^^ utf8format "\"%s\"" filename)
      ^^ break_1
      ^^ utf8format "is part of an %s cycle" kind
      |> group ),
    [(old_at, utf8format "Start of cyclic %s chain" kind)] )

let of_error = function
  (* IO errors *)
  | `Error_io (at, exn) -> ((at, exn |> Printexc.to_string |> utf8string), [])
  (* Syntax errors *)
  | `Error_lexeme (at, lexeme) | `Error_grammar (at, lexeme) -> (
    match lexeme with
    | "" -> ((at, utf8string "Syntax error"), [])
    | lexeme -> ((at, utf8format "Syntax error: %s" lexeme), []))
  | `Error_duplicated_label (at, l) ->
    ( (at, utf8string "Duplicated label " ^^ Label.pp l),
      [(Label.at l, utf8string "Initial"); (at, utf8string "Duplicate")] )
  | `Error_duplicated_typ_bind (at, i) ->
    ( (at, utf8string "Duplicated type binding " ^^ Typ.Var.pp i),
      [(Typ.Var.at i, utf8string "Initial"); (at, utf8string "Duplicate")] )
  | `Error_duplicated_bind (at, i) ->
    ( (at, utf8string "Duplicated binding " ^^ Exp.Var.pp i),
      [(Exp.Var.at i, utf8string "Initial"); (at, utf8string "Duplicate")] )
  (* Source errors *)
  | `Error_file_doesnt_exist (at, filename) ->
    ( ( at,
        utf8string "File"
        ^^ nest 2 (break_1 ^^ utf8format "\"%s\"" filename)
        ^^ break_1 ^^ utf8string "doesn't exist"
        |> group ),
      [(at, utf8format "File doesn't exist")] )
  | `Error_cyclic_includes (new_at, filename, old_at) ->
    cyclic "include" new_at filename old_at
  | `Error_cyclic_imports (new_at, filename, old_at) ->
    cyclic "import" new_at filename old_at
  (* Kind errors *)
  | `Error_kind_mismatch (at, expected_kind, actual_kind) ->
    ( ( at,
        utf8string "Expected type to have kind"
        ^^ nest 2 (break_1 ^^ Kind.pp expected_kind)
        ^^ break_1
        ^^ utf8string "but the type has kind"
        ^^ nest 2 (break_1 ^^ Kind.pp actual_kind)
        |> group ),
      [
        (at, utf8string "Kind mismatch");
        (Kind.at expected_kind, utf8string "Expected type");
      ] )
  | `Error_cyclic_kind at ->
    ((at, utf8string "cyclic kind"), [(at, utf8string "cyclic kind")])
  | `Error_mu_nested (at, typ, arg) ->
    ( ( at,
        utf8string "Nested types like"
        ^^ nest 2 (break_1 ^^ Typ.pp typ)
        ^^ break_1
        ^^ utf8string "are not allowed to keep type checking decidable"
        |> group ),
      [(Typ.at arg, utf8string "Nested argument passed to μ type constructor")]
    )
  | `Error_mu_non_contractive (at, typ, arg) ->
    ( ( at,
        utf8string "Non-contractive types like"
        ^^ nest 2 (break_1 ^^ Typ.pp typ)
        ^^ break_1
        ^^ utf8string "are not allowed"
        |> group ),
      [(Typ.at arg, utf8string "Non-contractive apply of μ type constructor")]
    )
  | `Error_typ_var_unbound (at, id) ->
    ( (at, utf8string "Unbound type variable " ^^ Typ.Var.pp id),
      [(Typ.Var.at id, utf8string "Unbound type variable")] )
  (* Type errors *)
  | `Error_var_unbound (at, id) ->
    ( (at, utf8string "Unbound variable " ^^ Exp.Var.pp id),
      [(Exp.Var.at id, FomPP.utf8string "Unbound variable")] )
  | `Error_typ_mismatch (at, expected_typ, actual_typ) ->
    ( ( at,
        utf8string "Expected expression to have type"
        ^^ nest 2 (break_1 ^^ Typ.pp expected_typ)
        ^^ break_1
        ^^ utf8string "but the expression has type"
        ^^ nest 2 (break_1 ^^ Typ.pp actual_typ)
        |> group ),
      [
        (at, utf8string "Type mismatch");
        (Typ.at expected_typ, utf8string "Expected type");
      ] )
  | `Error_typ_unexpected (at, mnemo, typ) ->
    ( ( at,
        utf8format "Expected a %s type but the expression has type" mnemo
        ^^ nest 2 (break_1 ^^ Typ.pp typ)
        |> group ),
      [(at, utf8format "Expected a %s type" mnemo)] )
  | `Error_product_lacks (at, typ, label) ->
    ( ( at,
        utf8string "Expected expression to have a type of the form"
        ^^ nest 2
             (break_1 ^^ utf8string "{" ^^ Label.pp label
            ^^ utf8string ": _, _}")
        ^^ break_1
        ^^ utf8string "but the expression has type"
        ^^ nest 2 (break_1 ^^ Typ.pp typ)
        |> group ),
      [(at, utf8string "Product lacks label")] )
  | `Error_label_missing (at, label, l_typ, m_typ) ->
    ( ( at,
        utf8string "Label"
        ^^ nest 2 (break_1 ^^ Label.pp label)
        ^^ break_1
        ^^ utf8string "missing from type"
        ^^ nest 2 (break_1 ^^ Typ.pp m_typ)
        ^^ break_1
        ^^ utf8string "to match the type"
        ^^ nest 2 (break_1 ^^ Typ.pp l_typ)
        |> group ),
      [(Typ.at m_typ, utf8string "Label missing")] )
  | `Error_typ_var_escapes (at, i, t) ->
    ( ( at,
        utf8string "The ∃ type variable"
        ^^ nest 2 (break_1 ^^ Typ.Var.pp i)
        ^^ break_1
        ^^ utf8string "escapes as part of the type"
        ^^ nest 2 (break_1 ^^ Typ.pp t)
        ^^ break_1
        ^^ utf8string "of the expression"
        |> group ),
      [(Typ.Var.at i, utf8string "∃ type variable")] )
