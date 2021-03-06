open FomPP
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

  let lit_bool at value =
    `Const (at, if value then Const.lit_true else Const.lit_false)

  (* Formatting *)

  let prec_min = 0

  let precedence (c : Exp.Const.t) =
    match c with
    | `LitBool _ | `LitNat _ | `LitString _ | `OpArithMinus | `OpArithPlus
    | `OpLogicalNot ->
      `Pre
    | `OpLogicalOr -> `Left 1
    | `OpLogicalAnd -> `Left 2
    | `OpEq _ | `OpEqNot _ -> `Non 3
    | `OpCmpGt | `OpCmpGtEq | `OpCmpLt | `OpCmpLtEq -> `Non 4
    | `OpArithAdd | `OpArithSub -> `Left 6
    | `OpArithDiv | `OpArithMul | `OpArithRem -> `Left 7

  let prec_case = 5
  let prec_uop = 8
  let prec_app = 9
  let prec_max = 10

  (* *)

  let some_empties = Some empties
  let some_spaces = Some spaces

  let rec hanging = function
    | `Lam _ | `Mu (_, `Lam _) | `Gen _ | `Product _ | `Inject _ -> some_spaces
    | `Case (_, _, cs) -> hanging cs
    | `Pack _ -> some_empties
    | _ -> None

  let rec pp prec_outer (e : t) =
    match e with
    | `Const (_, c) -> Const.pp c
    | `Var (_, i) -> Id.pp i
    | `Lam (_, i, t, e) ->
      [
        [lambda_lower; Id.pp i; colon; Typ.pp t; dot] |> concat;
        (match hanging e with
        | Some _ -> pp prec_min e
        | None -> [break_0; pp prec_min e] |> concat |> nest 2 |> group);
      ]
      |> concat
    | `App (_, `App (_, `Const (_, c), x), y) -> (
      match precedence c with
      | `Left prec ->
        [pp prec x; break_1; Const.pp c; space; pp (prec + 1) y]
        |> concat
        |> if prec < prec_outer then egyptian parens 2 else group
      | `Non prec ->
        [pp (prec + 1) x; break_1; Const.pp c; space; pp (prec + 1) y]
        |> concat
        |> if prec < prec_outer then egyptian parens 2 else group
      | `Pre ->
        [Const.pp c; break_1; pp prec_app x; break_1; pp prec_max y] |> concat)
    | `App (_, `Const (_, c), x) ->
      [Const.pp c; pp prec_uop x]
      |> concat
      |> if prec_uop < prec_outer then egyptian parens 2 else group
    | `App (_, f, x) ->
      [pp prec_app f; break_1; pp prec_max x]
      |> concat
      |> if prec_app < prec_outer then egyptian parens 2 else group
    | `Gen (_, i, k, e) ->
      [
        [lambda_upper; Typ.Id.pp i; Kind.pp_annot k; dot] |> concat;
        (match hanging e with
        | Some _ -> pp prec_min e
        | None -> [break_0; pp prec_min e] |> concat |> nest 2 |> group);
      ]
      |> concat
      |> if prec_min < prec_outer then egyptian parens 2 else group
    | `Inst (_, e, t) ->
      [pp prec_app e; Typ.pp t |> egyptian brackets 2]
      |> concat
      |> if prec_app < prec_outer then egyptian parens 2 else group
    | `LetIn (_, i, v, e) ->
      [
        [let_space; Id.pp i; space_equals] |> concat;
        (match hanging v with
        | Some (lhs, rhs) -> [lhs; pp prec_min v; rhs; in'] |> concat
        | None ->
          [break_1; pp prec_min v; space_in] |> concat |> nest 2 |> group);
        break_1;
        pp prec_min e;
      ]
      |> concat
      |> if prec_min < prec_outer then egyptian parens 2 else group
    | `LetTypIn (_, i, t, e) ->
      [
        [let_space; type'; space; Typ.Id.pp i; space_equals] |> concat;
        (match Typ.hanging t with
        | Some (lhs, rhs) -> [lhs; Typ.pp t; rhs; in'] |> concat
        | None -> [break_1; Typ.pp t; space_in] |> concat |> nest 2 |> group);
        break_1;
        pp prec_min e;
      ]
      |> concat
      |> if prec_min < prec_outer then egyptian parens 2 else group
    | `Mu (_, `Lam (_, i, t, e)) ->
      [
        [mu_lower; Id.pp i; colon; Typ.pp t; dot] |> concat;
        (match hanging e with
        | Some _ -> pp prec_min e
        | None -> [break_0; pp prec_min e] |> concat |> nest 2 |> group);
      ]
      |> concat
      |> if prec_min < prec_outer then egyptian parens 2 else group
    | `Mu (_, e) -> [mu_lower; pp prec_min e |> egyptian parens 2] |> concat
    | `IfElse (_, c, t, e) ->
      [
        [if'; space; pp prec_min c |> align; space; then'] |> concat;
        [break_1; pp prec_min t] |> concat |> nest 2;
        [break_1; else'] |> concat;
        [break_1; pp prec_min e] |> concat |> nest 2;
      ]
      |> concat
      |> if prec_min < prec_outer then egyptian parens 2 else group
    | `Product (_, fs) ->
      fs
      |> List.map (fun (l, e) ->
             [
               Label.pp l;
               space_equals;
               (match hanging e with
               | Some (lhs, _) -> [lhs; pp prec_min e] |> concat
               | None -> [break_1; pp prec_min e] |> concat |> nest 2 |> group);
             ]
             |> concat)
      |> separate comma_break_1 |> egyptian braces 2
    | `Select (_, e, l) -> [pp prec_max e; dot; Label.pp l] |> concat
    | `Inject (_, l, e, t) ->
      [
        [Label.pp l; space_equals] |> concat;
        (match hanging e with
        | Some (lhs, rhs) -> [lhs; pp prec_min e; rhs; colon] |> concat
        | None -> [break_1; pp prec_min e; space; colon] |> concat |> nest 2);
        (match Typ.hanging t with
        | Some (lhs, _) -> [lhs; Typ.pp t] |> concat
        | None -> [break_1; Typ.pp t] |> concat |> group);
      ]
      |> concat |> egyptian brackets 2
    | `Case (_, s, cs) ->
      [pp prec_case s; space; case'; space; pp prec_case cs]
      |> concat
      |> if prec_case < prec_outer then egyptian parens 2 else group
    | `Pack (_, t, e, x) ->
      [pp prec_min e; colon; space; Typ.pp x; slash; Typ.pp t]
      |> concat |> egyptian double_angles 2
    | `UnpackIn (_, ti, ei, v, e) ->
      [
        [
          let';
          [Id.pp ei; slash; Typ.Id.pp ti] |> concat |> egyptian double_angles 2;
          equals;
        ]
        |> concat;
        (match hanging v with
        | Some (lhs, rhs) -> [lhs; pp prec_min v; rhs; in'] |> concat
        | None ->
          [break_1; pp prec_min v; space_in] |> concat |> nest 2 |> group);
        [break_1; pp prec_min e] |> concat |> group;
      ]
      |> concat
      |> if prec_min < prec_outer then egyptian parens 2 else group
    | `Target (_, t, s) ->
      [target'; Typ.pp t |> egyptian brackets 2; space; Const.pp (`LitString s)]
      |> concat

  let pp = pp prec_min
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
