open FomBasis

module Name : sig
  type t = int

  val of_string : string -> t
  val to_string : t -> string
end = struct
  type t = int

  let id_to_int = Hashtbl.create 1000
  let int_to_id = Hashtbl.create 1000

  let of_string id =
    match Hashtbl.find_opt id_to_int id with
    | None ->
      let n = Hashtbl.length id_to_int in
      Hashtbl.replace id_to_int id n;
      Hashtbl.replace int_to_id n id;
      n
    | Some n -> n

  let to_string = Hashtbl.find int_to_id
end

module Counter : sig
  type t = int

  val next : unit -> int
end = struct
  type t = int

  let counter = ref 0

  let next () =
    let c = !counter + 1 in
    counter := c;
    c
end

module type S = sig
  type t = {name : Name.t; n : Counter.t; at : Loc.t}

  val at : t -> Loc.t
  val name : t -> Name.t

  (* Special *)

  val is_numeric : t -> bool
  val is_fresh : t -> bool

  (* Comparison *)

  val equal : t -> t -> bool
  val compare : t -> t -> int

  (* Formatting *)

  val to_string : t -> string
  val pp : t -> FomPP.document

  (* Constructors *)

  val of_string : Loc.t -> string -> t
  val of_name : Loc.t -> Name.t -> t

  (* Generated *)

  val fresh : Loc.t -> t

  (* Freshening *)

  val freshen : t -> t
end

module Make () : S = struct
  type t = {name : Name.t; n : Counter.t; at : Loc.t}

  let at {at; _} = at
  let name {name; _} = name

  let is_numeric {name; _} =
    let s = Name.to_string name in
    0 < String.length s && '0' <= s.[0] && s.[0] <= '9'

  (* Comparison *)

  let equal lhs rhs = lhs.name = rhs.name && lhs.n = rhs.n

  let compare lhs rhs =
    Stdlib.Int.compare lhs.name rhs.name <>? fun () ->
    Stdlib.Int.compare lhs.n rhs.n

  (* Formatting *)

  let to_string {name; n; _} =
    let it = Name.to_string name in
    if n = 0 then it else Printf.sprintf "%s$%d" it n

  let pp {name; n; _} =
    let it = Name.to_string name in
    if n = 0 then
      FomPP.utf8string it
    else
      FomPP.utf8format "%s$%d" it n

  (* Freshening *)

  let freshen {name; at; _} = {name; n = Counter.next (); at}

  (* Constructors *)

  let of_name at name = {name; n = 0; at}
  let of_string at s = of_name at (Name.of_string s)

  (* Generated *)

  let fresh = Name.of_string ""
  let is_fresh {name; _} = name = fresh
  let fresh at = {name = fresh; n = Counter.next (); at}
end
