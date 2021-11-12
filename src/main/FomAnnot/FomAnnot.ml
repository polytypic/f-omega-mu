open FomBasis
open FomSource
open FomAST

(* *)

module Annot = struct
  module LocSet = Set.Make (Loc)
  module LocMap = Map.Make (Loc)

  type map =
    < annot :
        [ `Label of Label.t * Typ.Core.t
        | `ExpId of Exp.Var.t * Typ.Core.t
        | `TypId of Typ.Var.t * Kind.t ]
    ; def : Loc.t
    ; uses : LocSet.t >
    LocMap.t

  type t = map MVar.t

  let field r : (t, _) Field.t = r#annotations
  let empty () = MVar.create LocMap.empty
  let scoping op = setting field (empty ()) op

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

  let merge lhs rhs =
    LocMap.merge
      ( Map.combining_with @@ fun lhs rhs ->
        make lhs#def (LocSet.union lhs#uses rhs#uses) lhs#annot )
      lhs rhs

  let add_def at annot =
    do_unless (Loc.is_empty at)
      (mutate field @@ LocMap.update at
      @@ function None -> Some (make at LocSet.empty annot) | some -> some)

  let add_use use def =
    do_unless (Loc.is_empty use)
      (mutate field @@ LocMap.update def @@ Option.map
      @@ fun o -> make o#def (LocSet.add use o#uses) o#annot)

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
      try_mutate field
        (LocMap.bindings
        >>> List.map_m (fun (at, v) ->
                match v#annot with
                | `TypId (id, kind) ->
                  let+ kind = resolve_kind kind in
                  (at, make v#def v#uses @@ `TypId (id, kind))
                | _ -> return (at, v))
        >-> LocMap.of_list)

    let def id kind = add_def (at id) @@ `TypId (id, kind)
    let use id = add_use @@ at id
  end
end
