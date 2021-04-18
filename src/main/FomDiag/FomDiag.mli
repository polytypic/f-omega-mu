open FomSource
open FomAST

module Error : sig
  (* Syntax errors *)

  val syntax : Loc.t -> string -> 'a
  val duplicated_label : Loc.t -> Label.t -> 'a

  (* Kind errors *)

  val kind_mismatch : Loc.t -> Kind.t -> Kind.t -> 'a
  val mu_kind : Loc.t -> Typ.t -> Kind.t -> 'a
  val mu_nested : Loc.t -> Typ.t -> Typ.t -> 'a
  val mu_non_contractive : Loc.t -> Typ.t -> Typ.t -> 'a
  val typ_var_unbound : Loc.t -> Typ.Id.t -> 'a
  val app_of_kind_star : Loc.t -> Typ.t -> Typ.t -> 'a
  val quantifier_kind : Loc.t -> FomPP.document -> Typ.t -> Kind.t -> 'a

  (* Type errors *)

  val var_unbound : Loc.t -> Exp.Id.t -> 'a
  val typ_of_kind_arrow : Loc.t -> Typ.t -> Kind.t -> 'a
  val typ_mismatch : Loc.t -> Typ.t -> Typ.t -> 'a
  val typ_unexpected : Loc.t -> string -> Typ.t -> 'a
  val product_lacks : Loc.t -> Typ.t -> Label.t -> 'a
  val label_missing : Loc.t -> Label.t -> Typ.t -> Typ.t -> 'a
  val typ_var_escapes : Loc.t -> Typ.Id.t -> Typ.t -> 'a
end
