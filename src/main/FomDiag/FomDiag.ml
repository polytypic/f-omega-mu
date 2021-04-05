open FomPP
open FomSource
open FomAST

module Error = struct
  (* Syntax errors *)

  let syntax at = function
    | "" -> Diagnostic.error (at, utf8string "Syntax error") []
    | lexeme -> Diagnostic.error (at, utf8format "Syntax error: %s" lexeme) []

  let duplicated_label at l =
    Diagnostic.error
      (at, concat [utf8string "Duplicated label "; Label.pp l])
      [(Label.at l, utf8string "Initial"); (at, utf8string "Duplicate")]

  (* Kind errors *)

  let kind_mismatch at expected_kind actual_kind =
    Diagnostic.error
      ( at,
        [
          utf8string "Expected type to have kind";
          [break_1; Kind.pp expected_kind] |> concat |> nest 2;
          break_1;
          utf8string "but the type has kind";
          [break_1; Kind.pp actual_kind] |> concat |> nest 2;
        ]
        |> concat |> group )
      [
        (at, utf8string "Kind mismatch");
        (Kind.at expected_kind, utf8string "Expected type");
      ]

  let mu_kind at typ kind =
    Diagnostic.error
      ( at,
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
        |> concat |> group )
      [(Kind.at kind, utf8string "Invalid kind for μ type constructor")]

  let mu_nested at typ arg =
    Diagnostic.error
      ( at,
        [
          utf8string "Nested types like";
          [break_1; Typ.pp typ] |> concat |> nest 2;
          break_1;
          utf8string "are not allowed to keep type checking decidable";
        ]
        |> concat |> group )
      [(Typ.at arg, utf8string "Nested argument passed to μ type constructor")]

  let mu_non_contractive at typ arg =
    Diagnostic.error
      ( at,
        [
          utf8string "Non-contractive types like";
          [break_1; Typ.pp typ] |> concat |> nest 2;
          break_1;
          utf8string "are not allowed";
        ]
        |> concat |> group )
      [(Typ.at arg, utf8string "Non-contractive apply of μ type constructor")]

  let typ_var_unbound at id =
    Diagnostic.error
      (at, concat [utf8string "Unbound type variable "; Typ.Id.pp id])
      [(Typ.Id.at id, utf8string "Unbound type variable")]

  let app_of_kind_star at fn arg =
    Diagnostic.error
      ( at,
        [
          utf8string "Type";
          [break_1; Typ.pp fn] |> concat |> nest 2;
          break_1;
          utf8string "does not take parameters but is given an argument of type";
          [break_1; Typ.pp arg] |> concat |> nest 2;
        ]
        |> concat |> group )
      [
        (Typ.at fn, utf8string "Type does not take parameters");
        (Typ.at arg, utf8string "Actual argument");
      ]

  let quantifier_kind at quantifier typ kind =
    Diagnostic.error
      ( at,
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
        |> concat |> group )
      [
        ( Typ.at typ,
          [
            utf8string "Invalid kind for ";
            quantifier;
            utf8string " type constructor";
          ]
          |> concat );
      ]

  (* Type errors *)

  let var_unbound at id =
    Diagnostic.error
      (at, concat [utf8string "Unbound variable "; Exp.Id.pp id])
      [(Exp.Id.at id, FomPP.utf8string "Unbound variable")]

  let typ_of_kind_arrow at typ kind =
    Diagnostic.error
      ( at,
        [
          utf8string "Type of kind * expected but type constructor";
          [break_1; Typ.pp typ] |> concat |> nest 2;
          break_1;
          utf8string "has kind";
          [break_1; Kind.pp kind] |> concat |> nest 2;
        ]
        |> concat |> group )
      [(Typ.at typ, utf8string "Invalid kind for type")]

  let typ_mismatch at expected_typ actual_typ =
    Diagnostic.error
      ( at,
        [
          utf8string "Expected expression to have type";
          [break_1; Typ.pp expected_typ] |> concat |> nest 2;
          break_1;
          utf8string "but the expression has type";
          [break_1; Typ.pp actual_typ] |> concat |> nest 2;
        ]
        |> concat |> group )
      [
        (at, utf8string "Type mismatch");
        (Typ.at expected_typ, utf8string "Expected type");
      ]

  let typ_unexpected at mnemo typ =
    Diagnostic.error
      ( at,
        [
          utf8format "Expected a %s type but the expression has type" mnemo;
          [break_1; Typ.pp typ] |> concat |> nest 2;
        ]
        |> concat |> group )
      [(at, utf8format "Expected a %s type" mnemo)]

  let product_lacks at typ label =
    Diagnostic.error
      ( at,
        [
          utf8string "Expected expression to have a type of the form";
          [break_1; utf8string "{"; Label.pp label; utf8string ": _, _}"]
          |> concat |> nest 2;
          break_1;
          utf8string "but the expression has type";
          [break_1; Typ.pp typ] |> concat |> nest 2;
        ]
        |> concat |> group )
      [(at, utf8string "Product lacks label")]

  let label_missing at label l_typ m_typ =
    Diagnostic.error
      ( at,
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
        |> concat |> group )
      [(Typ.at m_typ, utf8string "Label missing")]

  let typ_var_escapes at i t =
    Diagnostic.error
      ( at,
        [
          utf8string "The ∃ type variable";
          [break_1; Typ.Id.pp i] |> concat |> nest 2;
          break_1;
          utf8string "escapes as part of the type";
          [break_1; Typ.pp t] |> concat |> nest 2;
          break_1;
          utf8string "of the expression";
        ]
        |> concat |> group )
      [(Typ.Id.at i, utf8string "∃ type variable")]
end
