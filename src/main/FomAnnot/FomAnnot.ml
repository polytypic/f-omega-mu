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

    let def ({at; _} as id) typ r =
      if not (Hashtbl.mem r#annotations at) then
        let uses = ref [] in
        Hashtbl.add r#annotations at
          (object
             method def = at

             method annot = `Label (id, typ)

             method uses = uses
          end)

    let use {at; _} {at = def; _} r =
      let o = Hashtbl.find r#annotations def in
      o#uses := at :: o#uses.contents
  end

  module Exp = struct
    open Exp.Id

    let def ({at; _} as id : Exp.Id.t) typ r =
      if not (Hashtbl.mem r#annotations at) then
        let uses = ref [] in
        Hashtbl.add r#annotations at
          (object
             method def = at

             method annot = `ExpId (id, typ)

             method uses = uses
          end)

    let use {at; _} {at = def; _} r =
      let o = Hashtbl.find r#annotations def in
      o#uses := at :: o#uses.contents
  end

  module Typ = struct
    open Typ.Id

    let def ({at; _} as id) kind r =
      if not (Hashtbl.mem r#annotations at) then
        let uses = ref [] in
        Hashtbl.add r#annotations at
          (object
             method def = at

             method annot = `TypId (id, kind)

             method uses = uses
          end)

    let alias ({at; _} as id) typ r =
      if not (Hashtbl.mem r#annotations at) then
        let uses = ref [] in
        Hashtbl.add r#annotations at
          (object
             method def = at

             method annot = `TypAlias (id, typ)

             method uses = uses
          end)

    let use {at; _} {at = def; _} r =
      let o = Hashtbl.find r#annotations def in
      o#uses := at :: o#uses.contents
  end
end
