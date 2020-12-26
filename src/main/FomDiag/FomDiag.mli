open FomSource
open FomSyntax

module Error : sig
  (* Syntax errprs *)

  val syntax : Loc.t -> string -> 'a
  val duplicated_label : Loc.t -> Label.t -> 'a

  (* Kind errors *)

  val mu_kind : Loc.t -> Typ.t -> Kind.t -> 'a
  val typ_var_unbound : Loc.t -> Typ.Id.t -> 'a
  val app_of_kind_star : Loc.t -> Typ.t -> Typ.t -> 'a
  val app_kind_mismatch : Loc.t -> Typ.t -> Kind.t -> Typ.t -> Kind.t -> 'a
  val quantifier_kind : Loc.t -> FomPP.document -> Typ.t -> Kind.t -> 'a

  (* Type errors *)

  val var_unbound : Loc.t -> Exp.Id.t -> 'a
  val typ_of_kind_arrow : Loc.t -> Typ.t -> Kind.t -> 'a
  val inst_of_non_for_all : Loc.t -> Exp.t -> Typ.t -> 'a
  val inst_kind_mismatch : Loc.t -> Typ.t -> Kind.t -> Typ.t -> Kind.t -> 'a
  val typ_mismatch : Loc.t -> Typ.t -> Typ.t -> 'a
  val typ_non_arrow : Loc.t -> Typ.t -> 'a
  val typ_non_product : Loc.t -> Typ.t -> 'a
  val typ_non_sum : Loc.t -> Typ.t -> 'a
  val product_lacks : Loc.t -> Typ.t -> Label.t -> 'a
  val sum_lacks : Loc.t -> Typ.t -> Label.t -> 'a
  val labels_mismatch : Loc.t -> Label.t list -> Label.t list -> 'a
end
