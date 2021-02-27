open FomAST
open FomDiag
open FomSource

(* *)

module Kind = Kind
module Label = Label
module Typ = Typ

module Exp = struct
  module Const = Exp.Const
  module Id = Exp.Id

  type 't f = ['t Exp.f | `LetTypIn of Loc.t * Typ.Id.t * Typ.t * 't]
  type t = [ | t f]

  let at (e : _ f) =
    match e with `LetTypIn (at, _, _, _) -> at | #Exp.f as ast -> Exp.at ast

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
