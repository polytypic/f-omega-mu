open FomSource
open FomCST

val inc_ext : string
val sig_ext : string
val mod_ext : string

(* *)

val resolve : Loc.t -> LitString.t -> ext:string -> string

(* *)

val find_deps_defs :
  FomCST.Typ.t FomCST.Typ.Def.f list -> [`Include of Loc.t * LitString.t] list

val find_deps_typ : FomCST.Typ.t -> [`Include of Loc.t * LitString.t] list

val find_deps :
  FomCST.Exp.t ->
  [`Include of Loc.t * LitString.t | `Import of Loc.t * LitString.t] list
