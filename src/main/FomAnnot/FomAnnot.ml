open FomBasis
open FomSource
open FomAST

(* *)

open Rea

module Annot = struct
  module LocSet = Set.Make (Loc)

  type t =
    ( Loc.t,
      < annot :
          [ `Label of Label.t * Typ.t
          | `ExpId of Exp.Id.t * Typ.t
          | `TypId of Typ.Id.t * Kind.t ]
      ; def : Loc.t
      ; uses : LocSet.t ref > )
    Hashtbl.t

  let field r = r#annotations

  class con (annotations : t) =
    object
      method annotations = annotations
    end

  let empty () = Hashtbl.create ~random:true 1000

  module Label = struct
    open Label

    let def id typ =
      let+ annot = env_as field in
      let at = at id in
      if
        (not (is_fresh id))
        && (not (is_numeric id))
        && not (Hashtbl.mem annot at)
      then
        let uses = ref LocSet.empty in
        Hashtbl.replace annot at
          (object
             method def = at
             method annot = `Label (id, typ)
             method uses = uses
          end)

    let use id def =
      let+ annot = env_as field in
      let at = at id in
      if (not (is_fresh id)) && not (is_numeric id) then
        let o = Hashtbl.find annot def in
        o#uses := LocSet.add at o#uses.contents
  end

  module Exp = struct
    open Exp.Id

    let def id typ =
      let+ annot = env_as field in
      let at = at id in
      if
        (not (is_fresh id))
        && (not (is_numeric id))
        && not (Hashtbl.mem annot at)
      then
        let uses = ref LocSet.empty in
        Hashtbl.replace annot at
          (object
             method def = at
             method annot = `ExpId (id, typ)
             method uses = uses
          end)

    let use id def =
      let+ annot = env_as field in
      let at = at id in
      if (not (is_fresh id)) && not (is_numeric id) then
        let o = Hashtbl.find annot def in
        o#uses := LocSet.add at o#uses.contents
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
      let* annot = env_as field in
      annot |> Hashtbl.to_seq |> List.of_seq
      |> MList.iter @@ fun (at, v) ->
         match v#annot with
         | `TypId (id, kind) ->
           let+ kind = resolve_kind kind in
           Hashtbl.replace annot at @@ with_annot v @@ `TypId (id, kind)
         | _ -> unit

    let def id kind =
      let+ annot = env_as field in
      let at = at id in
      if not (Hashtbl.mem annot at) then
        let uses = ref LocSet.empty in
        Hashtbl.replace annot at
          (object
             method def = at
             method annot = `TypId (id, kind)
             method uses = uses
          end)

    let use' id def annot =
      let at = at id in
      match Hashtbl.find_opt annot def with
      | None -> ()
      | Some o -> o#uses := LocSet.add at o#uses.contents

    let use id def =
      let+ annot = env_as field in
      use' id def annot
  end
end
