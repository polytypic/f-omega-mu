module type S = sig
  type t = {it : string; at : Loc.t}

  val equal : t -> t -> bool
  val compare : t -> t -> int

  (* Formatting *)

  val pp : t -> FomPP.document

  (* Constructors *)

  val id : Loc.t -> string -> t

  (* Freshening *)

  val freshen : t -> t
end

module Counter : sig
  val next : unit -> int
end = struct
  let counter = ref (-1)

  let next () =
    let c = !counter + 1 in
    counter := c;
    c
end

module Make () : S = struct
  type t = {it : string; at : Loc.t}

  let equal lhs rhs = Stdlib.String.equal lhs.it rhs.it
  let compare lhs rhs = Stdlib.String.compare lhs.it rhs.it

  (* Formatting *)

  let pp {it; _} = FomPP.utf8string it

  (* Constructors *)

  let id at it = {it; at}

  (* Freshening *)

  let freshen {it; at} =
    let c = Counter.next () in
    let base =
      match String.rindex_opt it '$' with
      | Some n -> String.sub it 0 n
      | None -> it
    in
    {it = base ^ "$" ^ Int.to_string c; at}
end
