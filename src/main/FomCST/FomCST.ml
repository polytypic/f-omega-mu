open FomBasis
open FomAST
open FomSource

(* *)

module Kind = Kind
module Label = Label
module Row = Row
module Tuple = Tuple

module Typ = struct
  include Typ

  module Def = struct
    type 't f =
      [ `TypPar of Loc.t * (Var.t * Kind.t * 't) list
      | `TypRec of Loc.t * (Var.t * Kind.t * 't) list
      | `Include of Loc.t * JsonString.t ]
  end

  type 't f =
    [ ('t, Kind.t) Typ.f
    | `LetDefIn of Loc.t * 't Def.f * 't
    | `Import of Loc.t * JsonString.t ]

  type t = t f

  let at = function
    | `LetDefIn (at, _, _) | `Import (at, _) -> at
    | #Typ.f as ast -> Typ.at ast

  module Defs = struct
    type 't f =
      [ 't Def.f
      | `In of Loc.t * 't Def.f * 't f
      | `LocalIn of Loc.t * 't Def.f * 't f ]
  end
end

module Exp = struct
  include Exp

  module Pat = struct
    type t =
      [ `Id of Loc.t * Var.t * Typ.t
      | `Product of Loc.t * (Label.t * [`Pat of t | `Ann of Typ.t]) list
      | `Pack of Loc.t * t * Typ.Var.t * Typ.t ]

    let check p =
      let rec collect (ts, is) = function
        | `Id (_, i, _) -> (ts, i :: is)
        | `Product (_, ps) ->
          ps
          |> List.fold_left
               (fun (ts, is) -> function
                 | l, `Ann _ -> (ts, Var.of_label l :: is)
                 | _, `Pat p -> collect (ts, is) p)
               (ts, is)
        | `Pack (_, p, t, _) -> collect (t :: ts, is) p
      in
      let ts, is = collect ([], []) p in
      let check_ts =
        ts |> List.find_dup_opt Typ.Var.compare |> function
        | None -> unit
        | Some (i2, i1) -> fail @@ `Error_duplicated_typ_bind (Typ.Var.at i2, i1)
      in
      let check_is =
        is |> List.find_dup_opt Var.compare |> function
        | None -> unit
        | Some (i2, i1) -> fail @@ `Error_duplicated_bind (Var.at i2, i1)
      in
      check_ts >> check_is

    let rec label_for = function
      | `Id (_, i, _) -> Var.to_label i
      | `Product (at, _) -> Label.fresh at
      | `Pack (_, p, _, _) -> label_for p

    let at = function
      | `Id (at, _, _) | `Product (at, _) | `Pack (at, _, _, _) -> at

    let tuple at = function
      | [p] -> p
      | ps -> `Product (at, ps |> Tuple.labels at |> Row.map @@ fun p -> `Pat p)
  end

  type 'e tstr_elem = [`Exp of Label.t * 'e | `Str of JsonString.t]

  type 'e f =
    [ ('e, Typ.t, Kind.t) Exp.f
    | `AppL of Loc.t * 'e * 'e
    | `AppR of Loc.t * 'e * 'e
    | `LetDefIn of Loc.t * Typ.t Typ.Def.f * 'e
    | `Import of Loc.t * JsonString.t
    | `LetPatPar of Loc.t * (Pat.t * Typ.t option * 'e) list * 'e
    | `LetPatRec of Loc.t * (Pat.t * 'e) list * 'e
    | `LamPat of Loc.t * Pat.t * 'e
    | `Tstr of Loc.t * Var.t * 'e tstr_elem list
    | `Annot of Loc.t * 'e * Typ.t ]

  type t = t f

  let at = function
    | `AppL (at, _, _)
    | `AppR (at, _, _)
    | `LetDefIn (at, _, _)
    | `Import (at, _)
    | `LetPatPar (at, _, _)
    | `LetPatRec (at, _, _)
    | `LamPat (at, _, _)
    | `Annot (at, _, _) ->
      at
    | #Exp.f as ast -> Exp.at ast
end
