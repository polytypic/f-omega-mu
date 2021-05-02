open FomSource
open FomCST

val inc_ext : string
val sig_ext : string
val mod_ext : string

(* *)

val ensure_ext : string -> string -> string

(* *)

val is_https : string -> bool
val resolve : Loc.t -> LitString.t -> string

(* *)

val find_deps_defs :
  FomCST.Typ.t FomCST.Typ.Def.f list ->
  [`Typ of [`Include of Loc.t * LitString.t | `Import of Loc.t * LitString.t]]
  list

val find_deps_typ :
  FomCST.Typ.t ->
  [`Typ of [`Include of Loc.t * LitString.t | `Import of Loc.t * LitString.t]]
  list

val find_deps :
  FomCST.Exp.t ->
  [ `Typ of [`Include of Loc.t * LitString.t | `Import of Loc.t * LitString.t]
  | `Exp of [`Import of Loc.t * LitString.t] ]
  list
