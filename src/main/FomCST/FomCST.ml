open FomBasis
open FomPP
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
      [ `TypPar of (Var.t * Kind.t * 't) list
      | `TypRec of (Var.t * Kind.t * 't) list
      | `Include of Loc.t * JsonString.t ]
  end

  type 't f =
    [ ('t, Kind.t) Typ.f
    | `Let of Loc.t * 't Def.f * 't
    | `Import of Loc.t * JsonString.t ]

  type t = t f

  let at = function
    | `Let (at, _, _) | `Import (at, _) -> at
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
      | `Product of Loc.t * [`Pat of t | `Ann of Typ.t] Row.t
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

    let rec pp : t -> document = function
      | `Id (_, i, _) -> Var.pp i
      | `Product (_, ls) ->
        if Row.is_tuple ls then
          ls
          |> List.map (snd >>> function `Pat p -> pp p | `Ann _ -> underscore)
          |> separate comma_break_1 |> egyptian parens 2
        else
          ls
          |> List.map (fun (l, p) ->
                 Label.pp l
                 ^^
                 match p with
                 | `Pat p -> space_equals_space ^^ pp p
                 | `Ann _ -> empty)
          |> separate comma_break_1 |> egyptian braces 2
      | `Pack (_, p, _, _) -> pp p

    let to_string = pp >>> FomPP.to_string

    let at = function
      | `Id (at, _, _) | `Product (at, _) | `Pack (at, _, _, _) -> at

    let tuple at = function
      | [p] -> p
      | ps -> `Product (at, ps |> Tuple.labels at |> Row.map @@ fun p -> `Pat p)
  end

  type 'e tstr_elem = [`Exp of Label.t * 'e | `Str of JsonString.t]

  module Def = struct
    type 'e f =
      [ Typ.t Typ.Def.f
      | `PatPar of (Pat.t * Typ.t option * 'e) list
      | `PatRec of (Pat.t * 'e) list ]
  end

  type 'e f =
    [ ('e, Typ.t, Kind.t) Exp.f
    | `AppL of Loc.t * 'e * 'e
    | `AppR of Loc.t * 'e * 'e
    | `Let of Loc.t * 'e Def.f * 'e
    | `Import of Loc.t * JsonString.t
    | `LamPat of Loc.t * Pat.t * 'e
    | `Tstr of Loc.t * Var.t * 'e tstr_elem list
    | `Annot of Loc.t * 'e * Typ.t ]

  type t = t f

  let at = function
    | `AppL (at, _, _)
    | `AppR (at, _, _)
    | `Let (at, _, _)
    | `Import (at, _)
    | `LamPat (at, _, _)
    | `Annot (at, _, _) ->
      at
    | #Exp.f as ast -> Exp.at ast
end
