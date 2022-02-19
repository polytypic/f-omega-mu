open FomSource
open FomAST

(* IO errors *)

type io_error = [`Error_io of Loc.t * exn]

(* Syntax errors *)

type lexeme = [`Error_lexeme of Loc.t * string]
type grammar = [`Error_grammar of Loc.t * string]
type duplicated_label = [`Error_duplicated_label of Loc.t * Label.t]
type duplicated_typ_bind = [`Error_duplicated_typ_bind of Loc.t * Typ.Var.t]
type duplicated_bind = [`Error_duplicated_bind of Loc.t * Exp.Var.t]

type syntax_errors =
  [lexeme | grammar | duplicated_label | duplicated_typ_bind | duplicated_bind]

(* Source errors *)

type file_doesnt_exist = [`Error_file_doesnt_exist of Loc.t * string]
type cyclic_includes = [`Error_cyclic_includes of Loc.t * string * Loc.t]
type cyclic_imports = [`Error_cyclic_imports of Loc.t * string * Loc.t]
type source_errors = [file_doesnt_exist | cyclic_includes | cyclic_imports]

(* Kind errors *)

type kind_mismatch = [`Error_kind_mismatch of Loc.t * Kind.t * Kind.t]
type cyclic_kind = [`Error_cyclic_kind of Loc.t]
type mu_nested = [`Error_mu_nested of Loc.t * Typ.t * Typ.t]
type mu_non_contractive = [`Error_mu_non_contractive of Loc.t * Typ.t * Typ.t]
type typ_var_unbound = [`Error_typ_var_unbound of Loc.t * Typ.Var.t]

type kind_errors =
  [ kind_mismatch
  | cyclic_kind
  | mu_nested
  | mu_non_contractive
  | typ_var_unbound ]

(* Type errors *)

type var_unbound = [`Error_var_unbound of Loc.t * Exp.Var.t]
type typ_mismatch = [`Error_typ_mismatch of Loc.t * Typ.t * Typ.t]
type typ_unexpected = [`Error_typ_unexpected of Loc.t * string * Typ.Core.t]
type product_lacks = [`Error_product_lacks of Loc.t * Typ.Core.t * Label.t]
type sum_lacks = [`Error_sum_lacks of Loc.t * Typ.Core.t * Label.t]

type label_missing =
  [`Error_label_missing of Loc.t * Label.t * Typ.Core.t * Typ.Core.t]

type typ_var_escapes = [`Error_typ_var_escapes of Loc.t * Typ.Var.t * Typ.Core.t]

type non_disjoint_merge =
  [`Error_non_disjoint_merge of Loc.t * Typ.Core.t * Typ.Core.t]

type pat_lacks_annot = [`Error_pat_lacks_annot of Loc.t]
type exp_lacks_annot = [`Error_exp_lacks_annot of Loc.t]

type type_errors =
  [ var_unbound
  | typ_mismatch
  | typ_unexpected
  | product_lacks
  | sum_lacks
  | label_missing
  | typ_var_escapes
  | non_disjoint_merge
  | pat_lacks_annot
  | exp_lacks_annot ]

type t = [io_error | syntax_errors | source_errors | kind_errors | type_errors]
