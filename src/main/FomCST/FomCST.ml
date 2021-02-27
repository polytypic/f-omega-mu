open FomAST
open FomDiag
open FomSource

(* *)
module Kind = Kind

(* *)
module Label = Label

(* *)
module Typ = Typ

module Exp = struct
  module Const = Exp.Const
  module Id = Exp.Id

  type t =
    [ `Const of Loc.t * Const.t
    | `Var of Loc.t * Id.t
    | `Lam of Loc.t * Id.t * Typ.t * t
    | `App of Loc.t * t * t
    | `Gen of Loc.t * Typ.Id.t * Kind.t * t
    | `Inst of Loc.t * t * Typ.t
    | `LetIn of Loc.t * Id.t * t * t
    | `LetTypIn of Loc.t * Typ.Id.t * Typ.t * t
    | `Mu of Loc.t * t
    | `IfElse of Loc.t * t * t * t
    | `Product of Loc.t * (Label.t * t) list
    | `Select of Loc.t * t * Label.t
    | `Inject of Loc.t * Label.t * t * Typ.t
    | `Case of Loc.t * t * t
    | `Pack of Loc.t * Typ.t * t * Typ.t
    | `UnpackIn of Loc.t * Typ.Id.t * Id.t * t * t
    | `Target of Loc.t * Typ.t * string ]

  let at = function
    | `Const (at, _)
    | `Var (at, _)
    | `Lam (at, _, _, _)
    | `App (at, _, _)
    | `Gen (at, _, _, _)
    | `Inst (at, _, _)
    | `LetIn (at, _, _, _)
    | `LetTypIn (at, _, _, _)
    | `Mu (at, _)
    | `IfElse (at, _, _, _)
    | `Product (at, _)
    | `Select (at, _, _)
    | `Inject (at, _, _, _)
    | `Case (at, _, _)
    | `Pack (at, _, _, _)
    | `UnpackIn (at, _, _, _, _)
    | `Target (at, _, _) ->
      at

  let var_of_label ({it; at} : Label.t) = `Var (at, Id.id at it)

  let bin_op at lhs op rhs =
    match op with
    | `Const (_, `OpLogicalAnd) ->
      `IfElse (at, lhs, rhs, `Const (at, Const.lit_false))
    | `Const (_, `OpLogicalOr) ->
      `IfElse (at, lhs, `Const (at, Const.lit_true), rhs)
    | _ -> `App (at, `App (at, op, lhs), rhs)

  let lit_bool at value =
    `Const (at, if value then Const.lit_true else Const.lit_false)
end

let check_lab_list fs =
  let rec check_dups = function
    | l1 :: (l2 :: _ as ls) ->
      if Label.equal l1 l2 then
        Error.duplicated_label l2.at l1;
      check_dups ls
    | _ -> fs
  in
  fs |> List.map fst |> List.stable_sort Label.compare |> check_dups
