open FomBasis
open FomSource
open FomAST

(* *)

open Rea

module Annot = struct
  module LocSet = Set.Make (Loc)
  module LocMap = Map.Make (Loc)

  type map =
    < annot :
        [ `Label of Label.t * Typ.t
        | `ExpId of Exp.Id.t * Typ.t
        | `TypId of Typ.Id.t * Kind.t ]
    ; def : Loc.t
    ; uses : LocSet.t >
    LocMap.t

  type t = map MVar.t

  let field r : (t, _) Field.t = r#annotations
  let empty () = MVar.create LocMap.empty

  let scoping op =
    let* current = get field in
    setting field (empty ())
      ( op >>= fun result ->
        let* newer = get field >>= MVar.get in
        MVar.mutate current (LocMap.merge MapExt.prefer_lhs newer)
        >> return result )

  class con (annotations : t) =
    object
      val annotations = annotations
      method annotations = Field.make annotations (fun v -> {<annotations = v>})
    end

  let new_def def annot =
    object
      method def = def
      method annot = annot
      method uses = LocSet.empty
    end

  let add_use use o =
    let annot = o#annot and def = o#def and uses = LocSet.add use o#uses in
    object
      method def = def
      method annot = annot
      method uses = uses
    end

  module Label = struct
    open Label

    let def id typ =
      let* annot = get field in
      MVar.mutate annot @@ fun annot ->
      let at = at id in
      if
        (not (is_fresh id))
        && (not (is_numeric id))
        && not (LocMap.mem at annot)
      then
        LocMap.add at (new_def at @@ `Label (id, typ)) annot
      else
        annot

    let use id def =
      let* annot = get field in
      MVar.mutate annot @@ fun annot ->
      let at = at id in
      if (not (is_fresh id)) && not (is_numeric id) then
        LocMap.update def (Option.map (add_use at)) annot
      else
        annot
  end

  module Exp = struct
    open Exp.Id

    let def id typ =
      let* annot = get field in
      MVar.mutate annot @@ fun annot ->
      let at = at id in
      if
        (not (is_fresh id))
        && (not (is_numeric id))
        && not (LocMap.mem at annot)
      then
        LocMap.add at (new_def at @@ `ExpId (id, typ)) annot
      else
        annot

    let use id def =
      let* annot = get field in
      MVar.mutate annot @@ fun annot ->
      let at = at id in
      if (not (is_fresh id)) && not (is_numeric id) then
        LocMap.update def (Option.map (add_use at)) annot
      else
        annot
  end

  module Typ = struct
    open Typ.Id

    let with_annot o annot =
      let def = o#def in
      let uses = o#uses in
      object
        method def = def
        method annot = annot
        method uses = uses
      end

    let resolve resolve_kind =
      let* annot = get field in
      MVar.try_mutate annot @@ fun annot ->
      annot |> LocMap.bindings
      |> MList.traverse (fun (at, v) ->
             match v#annot with
             | `TypId (id, kind) ->
               let+ kind = resolve_kind kind in
               (at, with_annot v @@ `TypId (id, kind))
             | _ -> return (at, v))
      >>- (List.to_seq >>> LocMap.of_seq)

    let def id kind =
      let* annot = get field in
      MVar.mutate annot @@ fun annot ->
      let at = at id in
      if not (LocMap.mem at annot) then
        LocMap.add at (new_def at @@ `TypId (id, kind)) annot
      else
        annot

    let use id def =
      let* annot = get field in
      MVar.mutate annot @@ fun annot ->
      let at = at id in
      match LocMap.find_opt def annot with
      | None -> annot
      | Some o -> LocMap.update def (Option.map (add_use at)) annot
  end
end
