open FomPP
open FomSource
open FomSyntax

module Error = struct
  (* Syntax errors *)

  let syntax at = function
    | "" -> Diagnostic.error (at, utf8string "Syntax error") []
    | lexeme ->
      Diagnostic.error (at, utf8format "Syntax error: \"…%s…\"" lexeme) []

  let duplicated_label at l =
    Diagnostic.error
      (at, concat [utf8string "Duplicated label "; Label.pp l])
      [(l.at, utf8string "Initial"); (at, utf8string "Duplicate")]

  (* Kind errors *)

  let mu_kind at typ kind =
    Diagnostic.error
      ( at,
        [
          utf8string "μ(_) requires a type constructor of kind";
          [break_1; utf8string "k→k"] |> concat |> nest 2;
          break_1;
          utf8string "but given type";
          [break_1; Typ.pp typ] |> concat |> nest 2;
          break_1;
          utf8string "has kind";
          [break_1; Kind.pp kind] |> concat |> nest 2;
        ]
        |> concat |> group )
      [(Kind.at kind, utf8string "Invalid kind for μ type constructor")]

  let typ_var_unbound at id =
    Diagnostic.error
      (at, concat [utf8string "Unbound type variable "; Typ.Id.pp id])
      [(id.at, utf8string "Unbound type variable")]

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
        (Typ.at fn, utf8string "Type does not take arguments");
        (Typ.at arg, utf8string "Actual argument");
      ]

  let app_kind_mismatch at fn dom arg arg_kind =
    Diagnostic.error
      ( at,
        [
          utf8string "Type constructor";
          [break_1; Typ.pp fn] |> concat |> nest 2;
          break_1;
          utf8string "takes a parameter of kind";
          [break_1; Kind.pp dom] |> concat |> nest 2;
          break_1;
          utf8string "but given argument type";
          [break_1; Typ.pp arg] |> concat |> nest 2;
          break_1;
          utf8string "has kind";
          [break_1; Kind.pp arg_kind] |> concat |> nest 2;
        ]
        |> concat |> group )
      [
        (Kind.at dom, utf8string "Formal parameter");
        (Typ.at arg, utf8string "Actual argument");
      ]

  let quantifier_kind at quantifier typ kind =
    Diagnostic.error
      ( at,
        [
          quantifier;
          utf8string "(_) requires a type constructor of kind";
          [break_1; utf8string "(_→*)→*"] |> concat |> nest 2;
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
      [(id.at, FomPP.utf8string "Unbound variable")]

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

  let inst_of_non_for_all at fn fn_typ =
    Diagnostic.error
      ( at,
        [
          utf8string "Instantiation requires a type of the form";
          [break_1; utf8string "∀_:_._"] |> concat |> nest 2;
          break_1;
          utf8string "but target has type";
          [break_1; Typ.pp fn_typ] |> concat |> nest 2;
        ]
        |> concat |> group )
      [(Exp.at fn, utf8string "Target of instantiation")]

  let inst_kind_mismatch at fn_typ dom arg arg_kind =
    Diagnostic.error
      ( at,
        [
          utf8string "Instantiation target has type";
          [break_1; Typ.pp fn_typ] |> concat |> nest 2;
          break_1;
          utf8string "and takes a parameter of kind";
          [break_1; Kind.pp dom] |> concat |> nest 2;
          break_1;
          utf8string "but given argument type";
          [break_1; Typ.pp arg] |> concat |> nest 2;
          break_1;
          utf8string "has kind";
          [break_1; Kind.pp arg_kind] |> concat |> nest 2;
        ]
        |> concat |> group )
      [
        (Kind.at dom, utf8string "Formal parameter");
        (Typ.at arg, utf8string "Actual argument");
      ]

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

  let typ_non_arrow at typ =
    Diagnostic.error
      ( at,
        [
          utf8string "Expected a function but the expression has type";
          [break_1; Typ.pp typ] |> concat |> nest 2;
        ]
        |> concat |> group )
      [(at, utf8string "Expected a function")]

  let typ_non_product at typ =
    Diagnostic.error
      ( at,
        [
          utf8string "Expected a product but the expression has type";
          [break_1; Typ.pp typ] |> concat |> nest 2;
        ]
        |> concat |> group )
      [(at, utf8string "Expected a product")]

  let typ_non_sum at typ =
    Diagnostic.error
      ( at,
        [
          utf8string "Expected a sum but the expression has type";
          [break_1; Typ.pp typ] |> concat |> nest 2;
        ]
        |> concat |> group )
      [(at, utf8string "Expected a sum")]

  let product_lacks at typ label =
    Diagnostic.error
      ( at,
        [
          utf8string "Expected expression to have a type of the form";
          [break_1; utf8string "{"; Label.pp label; utf8string ": _, …}"]
          |> concat |> nest 2;
          break_1;
          utf8string "but the expression has type";
          [break_1; Typ.pp typ] |> concat |> nest 2;
        ]
        |> concat |> group )
      [(at, utf8string "Product lacks label")]

  let sum_lacks at typ label =
    Diagnostic.error
      ( at,
        [
          utf8string "Expected expression to have a type of the form";
          [break_1; utf8string "["; Label.pp label; utf8string ": _, …]"]
          |> concat |> nest 2;
          break_1;
          utf8string "but the expression has type";
          [break_1; Typ.pp typ] |> concat |> nest 2;
        ]
        |> concat |> group )
      [
        (at, [utf8string "Sum lacks label "; Label.pp label] |> concat);
        (label.at, utf8string "Label not in sum type");
      ]

  let labels_mismatch at lhs rhs =
    let module Set = Set.Make (Label) in
    let lhs = Set.of_list lhs in
    let rhs = Set.of_list rhs in
    let diff xs ys =
      Set.diff xs ys |> Set.elements
      |> List.map (fun l ->
             (l.Label.at, concat [utf8string "Unmatched label "; Label.pp l]))
    in
    Diagnostic.error
      (at, utf8string "Labels do not match")
      (diff lhs rhs @ diff rhs lhs)
end
