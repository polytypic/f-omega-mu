open FomPP
open FomSource
open FomAST

module Error = struct
  (* IO errors *)

  type io_error = [`Error_io of Loc.t * exn]

  (* Syntax errors *)

  type lexeme = [`Error_lexeme of Loc.t * string]
  type grammar = [`Error_grammar of Loc.t * string]
  type duplicated_label = [`Error_duplicated_label of Loc.t * Label.t]
  type syntax_errors = [lexeme | grammar | duplicated_label]

  (* Source errors *)

  type file_doesnt_exist = [`Error_file_doesnt_exist of Loc.t * string]
  type cyclic_includes = [`Error_cyclic_includes of Loc.t * string * Loc.t]
  type cyclic_imports = [`Error_cyclic_imports of Loc.t * string * Loc.t]
  type source_errors = [file_doesnt_exist | cyclic_includes | cyclic_imports]

  (* Kind errors *)

  type kind_mismatch = [`Error_kind_mismatch of Loc.t * Kind.t * Kind.t]
  type mu_kind = [`Error_mu_kind of Loc.t * Typ.t * Kind.t]
  type mu_nested = [`Error_mu_nested of Loc.t * Typ.t * Typ.t]
  type mu_non_contractive = [`Error_mu_non_contractive of Loc.t * Typ.t * Typ.t]
  type typ_var_unbound = [`Error_typ_var_unbound of Loc.t * Typ.Id.t]
  type app_of_kind_star = [`Error_app_of_kind_star of Loc.t * Typ.t * Typ.t]

  type quantifier_kind =
    [`Error_quantifier_kind of Loc.t * FomPP.document * Typ.t * Kind.t]

  type kind_errors =
    [ kind_mismatch
    | mu_kind
    | mu_nested
    | mu_non_contractive
    | typ_var_unbound
    | app_of_kind_star
    | quantifier_kind ]

  (* Type errors *)

  type var_unbound = [`Error_var_unbound of Loc.t * Exp.Id.t]
  type typ_of_kind_arrow = [`Error_typ_of_kind_arrow of Loc.t * Typ.t * Kind.t]
  type typ_mismatch = [`Error_typ_mismatch of Loc.t * Typ.t * Typ.t]
  type typ_unexpected = [`Error_typ_unexpected of Loc.t * string * Typ.t]
  type product_lacks = [`Error_product_lacks of Loc.t * Typ.t * Label.t]
  type label_missing = [`Error_label_missing of Loc.t * Label.t * Typ.t * Typ.t]
  type typ_var_escapes = [`Error_typ_var_escapes of Loc.t * Typ.Id.t * Typ.t]

  type type_errors =
    [ var_unbound
    | typ_of_kind_arrow
    | typ_mismatch
    | typ_unexpected
    | product_lacks
    | label_missing
    | typ_var_escapes ]

  (* Formatting *)

  type t = [io_error | syntax_errors | source_errors | kind_errors | type_errors]

  let cyclic kind new_at filename old_at =
    ( ( new_at,
        [
          utf8string "The file";
          [break_1; utf8format "\"%s\"" filename] |> concat |> nest 2;
          break_1;
          utf8format "is part of an %s cycle" kind;
        ]
        |> concat |> group ),
      [(old_at, utf8format "Start of cyclic %s chain" kind)] )

  let to_diagnostics = function
    (* IO errors *)
    | `Error_io (at, exn) -> ((at, exn |> Printexc.to_string |> utf8string), [])
    (* Syntax errors *)
    | `Error_lexeme (at, lexeme) | `Error_grammar (at, lexeme) -> (
      match lexeme with
      | "" -> ((at, utf8string "Syntax error"), [])
      | lexeme -> ((at, utf8format "Syntax error: %s" lexeme), []))
    | `Error_duplicated_label (at, l) ->
      ( (at, concat [utf8string "Duplicated label "; Label.pp l]),
        [(Label.at l, utf8string "Initial"); (at, utf8string "Duplicate")] )
    (* Source errors *)
    | `Error_file_doesnt_exist (at, filename) ->
      ( ( at,
          [
            utf8string "File";
            [break_1; utf8format "\"%s\"" filename] |> concat |> nest 2;
            break_1;
            utf8string "doesn't exist";
          ]
          |> concat |> group ),
        [(at, utf8format "File doesn't exist")] )
    | `Error_cyclic_includes (new_at, filename, old_at) ->
      cyclic "include" new_at filename old_at
    | `Error_cyclic_imports (new_at, filename, old_at) ->
      cyclic "import" new_at filename old_at
    (* Kind errors *)
    | `Error_kind_mismatch (at, expected_kind, actual_kind) ->
      ( ( at,
          [
            utf8string "Expected type to have kind";
            [break_1; Kind.pp expected_kind] |> concat |> nest 2;
            break_1;
            utf8string "but the type has kind";
            [break_1; Kind.pp actual_kind] |> concat |> nest 2;
          ]
          |> concat |> group ),
        [
          (at, utf8string "Kind mismatch");
          (Kind.at expected_kind, utf8string "Expected type");
        ] )
    | `Error_mu_kind (at, typ, kind) ->
      ( ( at,
          [
            utf8string "μ(_) requires a type constructor of kind";
            [break_1; utf8string "k → k"] |> concat |> nest 2;
            break_1;
            utf8string "but given type";
            [break_1; Typ.pp typ] |> concat |> nest 2;
            break_1;
            utf8string "has kind";
            [break_1; Kind.pp kind] |> concat |> nest 2;
          ]
          |> concat |> group ),
        [(Kind.at kind, utf8string "Invalid kind for μ type constructor")] )
    | `Error_mu_nested (at, typ, arg) ->
      ( ( at,
          [
            utf8string "Nested types like";
            [break_1; Typ.pp typ] |> concat |> nest 2;
            break_1;
            utf8string "are not allowed to keep type checking decidable";
          ]
          |> concat |> group ),
        [
          ( Typ.at arg,
            utf8string "Nested argument passed to μ type constructor" );
        ] )
    | `Error_mu_non_contractive (at, typ, arg) ->
      ( ( at,
          [
            utf8string "Non-contractive types like";
            [break_1; Typ.pp typ] |> concat |> nest 2;
            break_1;
            utf8string "are not allowed";
          ]
          |> concat |> group ),
        [
          (Typ.at arg, utf8string "Non-contractive apply of μ type constructor");
        ] )
    | `Error_typ_var_unbound (at, id) ->
      ( (at, concat [utf8string "Unbound type variable "; Typ.Id.pp id]),
        [(Typ.Id.at id, utf8string "Unbound type variable")] )
    | `Error_app_of_kind_star (at, fn, arg) ->
      ( ( at,
          [
            utf8string "Type";
            [break_1; Typ.pp fn] |> concat |> nest 2;
            break_1;
            utf8string
              "does not take parameters but is given an argument of type";
            [break_1; Typ.pp arg] |> concat |> nest 2;
          ]
          |> concat |> group ),
        [
          (Typ.at fn, utf8string "Type does not take parameters");
          (Typ.at arg, utf8string "Actual argument");
        ] )
    | `Error_quantifier_kind (at, quantifier, typ, kind) ->
      ( ( at,
          [
            quantifier;
            utf8string "(_) requires a type constructor of kind";
            [break_1; utf8string "(_ → *) → *"] |> concat |> nest 2;
            break_1;
            utf8string "but given type";
            [break_1; Typ.pp typ] |> concat |> nest 2;
            break_1;
            utf8string "has kind";
            [break_1; Kind.pp kind] |> concat |> nest 2;
          ]
          |> concat |> group ),
        [
          ( Typ.at typ,
            [
              utf8string "Invalid kind for ";
              quantifier;
              utf8string " type constructor";
            ]
            |> concat );
        ] )
    (* Type errors *)
    | `Error_var_unbound (at, id) ->
      ( (at, concat [utf8string "Unbound variable "; Exp.Id.pp id]),
        [(Exp.Id.at id, FomPP.utf8string "Unbound variable")] )
    | `Error_typ_of_kind_arrow (at, typ, kind) ->
      ( ( at,
          [
            utf8string "Type of kind * expected but type constructor";
            [break_1; Typ.pp typ] |> concat |> nest 2;
            break_1;
            utf8string "has kind";
            [break_1; Kind.pp kind] |> concat |> nest 2;
          ]
          |> concat |> group ),
        [(Typ.at typ, utf8string "Invalid kind for type")] )
    | `Error_typ_mismatch (at, expected_typ, actual_typ) ->
      ( ( at,
          [
            utf8string "Expected expression to have type";
            [break_1; Typ.pp expected_typ] |> concat |> nest 2;
            break_1;
            utf8string "but the expression has type";
            [break_1; Typ.pp actual_typ] |> concat |> nest 2;
          ]
          |> concat |> group ),
        [
          (at, utf8string "Type mismatch");
          (Typ.at expected_typ, utf8string "Expected type");
        ] )
    | `Error_typ_unexpected (at, mnemo, typ) ->
      ( ( at,
          [
            utf8format "Expected a %s type but the expression has type" mnemo;
            [break_1; Typ.pp typ] |> concat |> nest 2;
          ]
          |> concat |> group ),
        [(at, utf8format "Expected a %s type" mnemo)] )
    | `Error_product_lacks (at, typ, label) ->
      ( ( at,
          [
            utf8string "Expected expression to have a type of the form";
            [break_1; utf8string "{"; Label.pp label; utf8string ": _, _}"]
            |> concat |> nest 2;
            break_1;
            utf8string "but the expression has type";
            [break_1; Typ.pp typ] |> concat |> nest 2;
          ]
          |> concat |> group ),
        [(at, utf8string "Product lacks label")] )
    | `Error_label_missing (at, label, l_typ, m_typ) ->
      ( ( at,
          [
            utf8string "Label";
            [break_1; Label.pp label] |> concat |> nest 2;
            break_1;
            utf8string "missing from type";
            [break_1; Typ.pp m_typ] |> concat |> nest 2;
            break_1;
            utf8string "to match the type";
            [break_1; Typ.pp l_typ] |> concat |> nest 2;
          ]
          |> concat |> group ),
        [(Typ.at m_typ, utf8string "Label missing")] )
    | `Error_typ_var_escapes (at, i, t) ->
      ( ( at,
          [
            utf8string "The ∃ type variable";
            [break_1; Typ.Id.pp i] |> concat |> nest 2;
            break_1;
            utf8string "escapes as part of the type";
            [break_1; Typ.pp t] |> concat |> nest 2;
            break_1;
            utf8string "of the expression";
          ]
          |> concat |> group ),
        [(Typ.Id.at i, utf8string "∃ type variable")] )
end
