open FomSource
open FomAST

module Annot = struct
  type t =
    ( Loc.t,
      < annot :
          [ `Label of Label.t * Typ.t
          | `ExpId of Exp.Id.t * Typ.t
          | `TypId of Typ.Id.t * Kind.t
          | `TypAlias of Typ.Id.t * Typ.t ]
      ; def : Loc.t
      ; uses : Loc.t list ref > )
    Hashtbl.t

  let empty () = Hashtbl.create ~random:true 1000

  module Label = struct
    open Label

    let def id typ r =
      let at = at id in
      if
        (not (is_fresh id))
        && (not (is_numeric id))
        && not (Hashtbl.mem r#annotations at)
      then
        let uses = ref [] in
        Hashtbl.add r#annotations at
          (object
             method def = at

             method annot = `Label (id, typ)

             method uses = uses
          end)

    let use id def r =
      let at = at id in
      if (not (is_fresh id)) && not (is_numeric id) then
        let o = Hashtbl.find r#annotations def in
        o#uses := at :: o#uses.contents
  end

  module Exp = struct
    open Exp.Id

    let def id typ r =
      let at = at id in
      if
        (not (is_fresh id))
        && (not (is_numeric id))
        && not (Hashtbl.mem r#annotations at)
      then
        let uses = ref [] in
        Hashtbl.add r#annotations at
          (object
             method def = at

             method annot = `ExpId (id, typ)

             method uses = uses
          end)

    let use id def r =
      let at = at id in
      if (not (is_fresh id)) && not (is_numeric id) then
        let o = Hashtbl.find r#annotations def in
        o#uses := at :: o#uses.contents
  end

  module Typ = struct
    open Typ.Id

    let def id kind r =
      let at = at id in
      if not (Hashtbl.mem r#annotations at) then
        let uses = ref [] in
        Hashtbl.add r#annotations at
          (object
             method def = at

             method annot = `TypId (id, kind)

             method uses = uses
          end)

    let alias id typ r =
      let at = at id in
      if not (Hashtbl.mem r#annotations at) then
        let uses = ref [] in
        Hashtbl.add r#annotations at
          (object
             method def = at

             method annot = `TypAlias (id, typ)

             method uses = uses
          end)

    let use id def r =
      let at = at id in
      match Hashtbl.find_opt r#annotations def with
      | None -> ()
      | Some o -> o#uses := at :: o#uses.contents
  end
end
