open FomBasis
open FomAST
open FomDiag
open FomSource

(* *)

module Kind = Kind
module Label = Label

module Tuple = struct
  let labels at =
    List.mapi (fun i t -> (Label.of_string (at t) (Int.to_string (i + 1)), t))
end

module Typ = struct
  include Typ

  module Id = struct
    include Typ.Id

    let to_label i = Label.of_name (at i) (name i)
  end

  module Def = struct
    type 't f =
      [ `Typ of Loc.t * Id.t * Kind.t option * 't
      | `TypRec of Loc.t * ((Id.t * Kind.t) * 't) list ]
  end

  type 't f = ['t Typ.f | `LetDefIn of Loc.t * 't Def.f * 't]
  type t = [ | t f]

  let at = function `LetDefIn (at, _, _) -> at | #Typ.f as ast -> Typ.at ast
  let tuple at' = function [t] -> t | ts -> product at' (Tuple.labels at ts)
end

module Exp = struct
  module Const = Exp.Const

  module Id = struct
    include Exp.Id

    let to_label i = Label.of_name (at i) (name i)
  end

  module Pat = struct
    type t =
      [ `Id of Loc.t * Id.t * Typ.t
      | `Product of Loc.t * (Label.t * [`Pat of t | `Ann of Typ.t]) list
      | `Pack of Loc.t * t * Typ.Id.t * Typ.t ]

    let at = function
      | `Id (at, _, _) | `Product (at, _) | `Pack (at, _, _, _) -> at

    let tuple at' = function
      | [p] -> p
      | ps ->
        `Product
          ( at',
            ps |> Tuple.labels at |> List.map (Pair.map id @@ fun p -> `Pat p)
          )
  end

  type 'e f =
    [ ('e, Typ.t) Exp.f
    | `AppL of Loc.t * 'e * 'e
    | `AppR of Loc.t * 'e * 'e
    | `LetDefIn of Loc.t * Typ.t Typ.Def.f * 'e
    | `LetPat of Loc.t * Pat.t * Typ.t option * 'e * 'e
    | `LetPatRec of Loc.t * (Pat.t * 'e) list * 'e
    | `LamPat of Loc.t * Pat.t * 'e
    | `Annot of Loc.t * 'e * Typ.t ]

  type t = [ | t f]

  let at = function
    | `AppL (at, _, _)
    | `AppR (at, _, _)
    | `LetDefIn (at, _, _)
    | `LetPat (at, _, _, _, _)
    | `LetPatRec (at, _, _)
    | `LamPat (at, _, _)
    | `Annot (at, _, _) ->
      at
    | #Exp.f as ast -> Exp.at ast

  let tuple at' = function [e] -> e | es -> `Product (at', Tuple.labels at es)

  let lit_bool at value =
    `Const (at, if value then Const.lit_true else Const.lit_false)
end

let check_lab_list fs =
  let rec check_dups = function
    | l1 :: (l2 :: _ as ls) ->
      if Label.equal l1 l2 then
        Error.duplicated_label (Label.at l2) l1;
      check_dups ls
    | _ -> fs
  in
  fs |> List.map fst |> List.stable_sort Label.compare |> check_dups
