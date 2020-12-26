module Label = struct
  open FomSyntax.Label

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
  open FomSyntax.Exp.Id

  let def ({at; _} as id : FomSyntax.Exp.Id.t) typ r =
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
  open FomSyntax.Typ.Id

  let def ({at; _} as id) kind r =
    if not (Hashtbl.mem r#annotations at) then
      let uses = ref [] in
      Hashtbl.add r#annotations at
        (object
           method def = at

           method annot = `TypId (id, kind)

           method uses = uses
        end)

  let use {at; _} {at = def; _} r =
    let o = Hashtbl.find r#annotations def in
    o#uses := at :: o#uses.contents
end
