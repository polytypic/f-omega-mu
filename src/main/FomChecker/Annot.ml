module Label = struct
  open FomAST.Label

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
  open FomAST.Exp.Id

  let def ({at; _} as id : FomAST.Exp.Id.t) typ r =
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
  open FomAST.Typ.Id

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
