open FomBasis
open FomAST
open FomSource

(* *)

module Kind = Kind
module Label = Label
module Row = Row

module Tuple = struct
  let labels at =
    List.mapi (fun i t -> (Label.of_string (at t) (Int.to_string (i + 1)), t))
end

module Typ = struct
  include Typ

  module Var = struct
    include Typ.Var

    let to_label i = Label.of_name (at i) (name i)
  end

  module Def = struct
    type 't f =
      [ `Typ of Loc.t * Var.t * Kind.t * 't
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

  let tuple at' = function [t] -> t | ts -> product at' (Tuple.labels at ts)

  let atom l =
    let at' = Label.at l in
    sum at' [(l, tuple at' [])]
end

module Exp = struct
  include Exp

  module Var = struct
    include Exp.Var

    let to_label i = Label.of_name (at i) (name i)
  end

  module Pat = struct
    type t =
      [ `Id of Loc.t * Var.t * Typ.t
      | `Product of Loc.t * (Label.t * [`Pat of t | `Ann of Typ.t]) list
      | `Pack of Loc.t * t * Typ.Var.t * Typ.t ]

    let check p =
      let open Rea in
      let rec collect (ts, is) = function
        | `Id (_, i, _) -> (ts, i :: is)
        | `Product (_, ps) ->
          ps
          |> List.fold_left
               (fun (ts, is) -> function
                 | l, `Ann _ ->
                   (ts, Exp.Var.of_name (Label.at l) (Label.name l) :: is)
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

    let tuple at' = function
      | [p] -> p
      | ps -> `Product (at', ps |> Tuple.labels at |> Row.map @@ fun p -> `Pat p)
  end

  type 'e f =
    [ ('e, Typ.t, Kind.t) Exp.f
    | `AppL of Loc.t * 'e * 'e
    | `AppR of Loc.t * 'e * 'e
    | `LetDefIn of Loc.t * Typ.t Typ.Def.f * 'e
    | `Import of Loc.t * JsonString.t
    | `LetPat of Loc.t * Pat.t * Typ.t option * 'e * 'e
    | `LetPatRec of Loc.t * (Pat.t * 'e) list * 'e
    | `LamPat of Loc.t * Pat.t * 'e
    | `Annot of Loc.t * 'e * Typ.t ]

  type t = t f

  let at = function
    | `AppL (at, _, _)
    | `AppR (at, _, _)
    | `LetDefIn (at, _, _)
    | `Import (at, _)
    | `LetPat (at, _, _, _, _)
    | `LetPatRec (at, _, _)
    | `LamPat (at, _, _)
    | `Annot (at, _, _) ->
      at
    | #Exp.f as ast -> Exp.at ast

  let tuple at' = function [e] -> e | es -> `Product (at', Tuple.labels at es)

  let atom l =
    let at' = Label.at l in
    `Inject (at', l, tuple at' [])

  let lit_bool at value =
    `Const (at, if value then Const.lit_true else Const.lit_false)
end
