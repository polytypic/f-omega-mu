open FomPP
open FomBasis

module Name : sig
  type t = int

  val of_string : string -> t
  val to_string : t -> string

  (* *)

  val compare : t cmp

  (* *)

  val underscore : t
  val fresh : t
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

  (* *)

  let compare = Int.compare

  (* *)

  let underscore = of_string "_"
  let fresh = of_string ""
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
  val set_at : Loc.t -> t -> t
  val name : t -> Name.t

  (* Special *)

  val is_numeric : t -> bool
  val is_fresh : t -> bool
  val is_underscore : t -> bool

  (* Comparison *)

  val equal : t -> t -> bool
  val compare : t -> t -> int

  (* Formatting *)

  val to_string : t -> string
  val pp : ?hr:bool -> t -> document

  (* Constructors *)

  val underscore : Loc.t -> t
  val of_string : Loc.t -> string -> t
  val of_name : Loc.t -> Name.t -> t
  val of_number : Loc.t -> Bigint.t -> t

  (* Generated *)

  val fresh : Loc.t -> t

  (* Freshening *)

  val freshen : t -> t

  module Unsafe : sig
    val set_counter : int -> t -> t
  end
end

module Make () : S = struct
  type t = {name : Name.t; n : Counter.t; at : Loc.t}

  (* *)

  let at {at; _} = at
  let name {name; _} = name

  let is_numeric {name; _} =
    let s = Name.to_string name in
    0 < String.length s && '0' <= s.[0] && s.[0] <= '9'

  let set_at at {name; n; _} = {at; name; n}

  (* Comparison *)

  let equal lhs rhs = lhs.name = rhs.name && lhs.n = rhs.n

  let compare lhs rhs =
    Int.compare lhs.name rhs.name <>? fun () -> Int.compare lhs.n rhs.n

  (* Formatting *)

  let to_string {name; n; _} =
    let it = Name.to_string name in
    if n = 0 then it else Printf.sprintf "%s$%d" it n

  let pp ?(hr = true) {name; n; _} =
    let it = Name.to_string name |> utf8string in
    if n = 0 || name = Name.underscore then it
    else if hr then it ^^ subscript n
    else it ^^ utf8format "$%d" n

  (* Freshening *)

  let freshen {name; at; _} = {name; n = Counter.next (); at}

  (* Constructors *)

  let of_name at name =
    let n =
      if name = Name.underscore || name = Name.fresh then Counter.next () else 0
    in
    {name; n; at}

  let of_string at s = of_name at (Name.of_string s)
  let of_number at n = of_string at (Bigint.to_string n)

  (* Generated *)

  let is_fresh {name; n; _} =
    name = Name.fresh || (n <> 0 && (Name.to_string name).[0] = '_')

  let fresh at = {name = Name.fresh; n = Counter.next (); at}

  (* Underscore *)

  let is_underscore {name; _} = Name.underscore = name
  let underscore at = {name = Name.underscore; n = Counter.next (); at}

  module Unsafe = struct
    let set_counter n t = {t with n}
  end
end
