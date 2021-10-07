open FomBasis
open FomSource
open FomAST

(* *)

module Annot = struct
  module LocSet = Set.Make (Loc)
  module LocMap = Map.Make (Loc)

  type map =
    < annot :
        [ `Label of Label.t * Typ.t
        | `ExpId of Exp.Var.t * Typ.t
        | `TypId of Typ.Var.t * Kind.t ]
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
        MVar.mutate current (LocMap.merge Map.prefer_lhs newer) >> return result
      )

  class con (annotations : t) =
    object
      val annotations = annotations
      method annotations = Field.make annotations (fun v -> {<annotations = v>})
    end

  let make def uses annot =
    object
      method def = def
      method uses = uses
      method annot = annot
    end

  let add_def at annot =
    let* locmap = get field in
    MVar.mutate locmap @@ LocMap.update at
    @@ function None -> Some (make at LocSet.empty annot) | some -> some

  let add_use use def =
    let* locmap = get field in
    MVar.mutate locmap @@ LocMap.update def @@ Option.map
    @@ fun o -> make o#def (LocSet.add use o#uses) o#annot

  module Label = struct
    open Label

    let def id typ =
      if is_fresh id || is_numeric id then
        unit
      else
        add_def (at id) @@ `Label (id, typ)

    let use id def =
      if is_fresh id || is_numeric id then
        unit
      else
        add_use (at id) def
  end

  module Exp = struct
    open Exp.Var

    let def id typ =
      if is_fresh id || is_numeric id then
        unit
      else
        add_def (at id) @@ `ExpId (id, typ)

    let use id def =
      if is_fresh id || is_numeric id then
        unit
      else
        add_use (at id) def
  end

  module Typ = struct
    open Typ.Var

    let resolve resolve_kind =
      let* annot = get field in
      MVar.try_mutate annot @@ fun annot ->
      annot |> LocMap.bindings
      |> List.map_fr (fun (at, v) ->
             match v#annot with
             | `TypId (id, kind) ->
               let+ kind = resolve_kind kind in
               (at, make v#def v#uses @@ `TypId (id, kind))
             | _ -> return (at, v))
      >>- LocMap.of_list

    let def id kind = add_def (at id) @@ `TypId (id, kind)
    let use id = add_use @@ at id
  end
end
