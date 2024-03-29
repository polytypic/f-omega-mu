open Rea
open StdlibPlus
open FomPPrint
open FomAST
open FomSource

(* *)

module Kind = Kind
module Label = Label
module Row = Row
module Tuple = Tuple

module Aggr = struct
  let cons = "Cons"
  let nil = "Nil"
end

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
    | `Annot of Loc.t * 't * Kind.t
    | `Import of Loc.t * JsonString.t ]

  type t = t f

  let at = function
    | `Let (at, _, _) | `Annot (at, _, _) | `Import (at, _) -> at
    | #Typ.f as ast -> Typ.at ast

  module Defs = struct
    type 't f =
      [ 't Def.f
      | `In of Loc.t * 't Def.f * 't f
      | `LocalIn of Loc.t * 't Def.f * 't f ]
  end

  let aggr at' xs tl =
    List.fold_right
      (fun x ys -> sum at' [(Label.of_string at' Aggr.cons, tuple at' [x; ys])])
      xs
      (match tl with
      | Some typ -> typ
      | None -> atom (Label.of_string at' Aggr.nil))
end

module Exp = struct
  include Exp

  module Pat = struct
    type t =
      [ `Var of Loc.t * Var.t
      | `Const of Loc.t * [`Unit]
      | `Annot of Loc.t * t * Typ.t
      | `Product of Loc.t * t Row.t
      | `Pack of Loc.t * t * Typ.Var.t * Kind.t ]

    let check p =
      eta'0 @@ fun () ->
      let rec collect (ts, is) = function
        | `Var (_, i) -> (ts, i :: is)
        | `Const (_, `Unit) -> (ts, is)
        | `Annot (_, p, _) -> collect (ts, is) p
        | `Product (_, ps) ->
          ps
          |> List.fold_left (fun (ts, is) (_, p) -> collect (ts, is) p) (ts, is)
        | `Pack (_, p, t, _) -> collect (t :: ts, is) p
      in
      let ts, is = collect ([], []) p in
      let check_ts =
        ts
        |> List.find_dup_opt Typ.Var.compare
        |> Option.iter_er @@ fun (i2, i1) ->
           fail @@ `Error_duplicated_typ_bind (Typ.Var.at i2, i1)
      in
      let check_is =
        is
        |> List.find_dup_opt Var.compare
        |> Option.iter_er @@ fun (i2, i1) ->
           fail @@ `Error_duplicated_bind (Var.at i2, i1)
      in
      check_ts >> check_is

    let rec pp = function
      | `Var (_, i) -> Var.pp i
      | `Const (_, `Unit) -> unit'
      | `Annot (_, p, _) -> pp p
      | `Product (_, ls) ->
        if Row.is_tuple ls then
          ls
          |> List.map (snd >>> pp)
          |> separate comma_break_1 |> egyptian parens 2
        else
          ls
          |> List.map (function
               | l, `Var (_, i) when Label.equal l (Var.to_label i) ->
                 Label.pp l
               | l, p -> Label.pp l ^^ space_equals_space ^^ pp p)
          |> separate comma_break_1 |> egyptian braces 2
      | `Pack (_, p, _, _) -> pp p

    let to_string p = p |> pp |> to_string

    let at = function
      | `Var (at, _)
      | `Const (at, _)
      | `Annot (at, _, _)
      | `Product (at, _)
      | `Pack (at, _, _, _) ->
        at

    let tuple at = function
      | [] -> `Const (at, `Unit)
      | [p] -> p
      | ps -> `Product (at, Tuple.labels at ps)
  end

  type 'e tstr_elem = [`Exp of Label.t * 'e | `Str of JsonString.t]

  module Def = struct
    type 'e f =
      [ Typ.t Typ.Def.f
      | `PatPar of (Pat.t * 'e) list
      | `PatRec of (Pat.t * 'e) list ]
  end

  type 'e f =
    [ ('e, Typ.t, Kind.t) Exp.f
    | `AppL of Loc.t * 'e * 'e
    | `AppR of Loc.t * 'e * 'e
    | `Let of Loc.t * 'e Def.f * 'e
    | `Seq of Loc.t * 'e * 'e
    | `Import of Loc.t * JsonString.t
    | `LamPat of Loc.t * Pat.t * 'e
    | `Tstr of Loc.t * Var.t * 'e tstr_elem list
    | `Annot of Loc.t * 'e * Typ.t ]

  type t = t f

  let at = function
    | `AppL (at, _, _)
    | `AppR (at, _, _)
    | `Let (at, _, _)
    | `Seq (at, _, _)
    | `Import (at, _)
    | `LamPat (at, _, _)
    | `Tstr (at, _, _)
    | `Annot (at, _, _) ->
      at
    | #Exp.f as ast -> Exp.at ast

  let aggr at' xs tl =
    List.fold_right
      (fun x ys ->
        `Inject (at', Label.of_string at' Aggr.cons, Exp.tuple at' [x; ys]))
      xs
      (match tl with
      | Some exp -> exp
      | None -> Exp.atom (Label.of_string at' Aggr.nil))
end

module Annot = struct
  let opt at = Option.fold ~none:id ~some:(fun a x -> `Annot (at a, x, a))
end
