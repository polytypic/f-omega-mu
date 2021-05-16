open FomSource
open FomAST

module Error : sig
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

  val to_diagnostics : [< t] -> Diagnostic.t * Diagnostic.t list
end
