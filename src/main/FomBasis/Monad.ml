module type Monad = sig
  type (-'I, 'T, +'O, +'a) m

  val return : 'a -> ('I, 'T, 'O, 'a) m

  val ( let* ) :
    ('I, 'T, 'O, 'a) m -> ('a -> ('I, 'T, 'O, 'b) m) -> ('I, 'T, 'O, 'b) m

  val ( let+ ) : ('I, 'T, 'O, 'a) m -> ('a -> 'b) -> ('I, 'T, 'O, 'b) m
end

module type S = sig
  include Monad

  (* *)

  val ( >> ) : ('I, 'T, 'O, unit) m -> ('I, 'T, 'O, 'a) m -> ('I, 'T, 'O, 'a) m

  val ( >>= ) :
    ('I, 'T, 'O, 'a) m -> ('a -> ('I, 'T, 'O, 'b) m) -> ('I, 'T, 'O, 'b) m

  val ( >>- ) : ('I, 'T, 'O, 'a) m -> ('a -> 'b) -> ('I, 'T, 'O, 'b) m

  (* *)

  val lift1 : ('d1 -> 'c) -> ('I, 'T, 'O, 'd1) m -> ('I, 'T, 'O, 'c) m

  val lift2 :
    ('d1 -> 'd2 -> 'c) ->
    ('I, 'T, 'O, 'd1) m ->
    ('I, 'T, 'O, 'd2) m ->
    ('I, 'T, 'O, 'c) m

  (* *)

  val ( &&& ) :
    ('I, 'T, 'O, bool) m -> ('I, 'T, 'O, bool) m -> ('I, 'T, 'O, bool) m

  val ( ||| ) :
    ('I, 'T, 'O, bool) m -> ('I, 'T, 'O, bool) m -> ('I, 'T, 'O, bool) m

  (* *)

  module MList : sig
    val fold_left :
      ('a -> 'b -> ('I, 'T, 'O, 'a) m) -> 'a -> 'b list -> ('I, 'T, 'O, 'a) m

    val fold_left2 :
      ('a -> 'b -> 'c -> ('I, 'T, 'O, 'a) m) ->
      'a ->
      'b list ->
      'c list ->
      ('I, 'T, 'O, 'a) m

    (* *)

    val iter : ('a -> ('I, 'T, 'O, unit) m) -> 'a list -> ('I, 'T, 'O, unit) m

    val iter2 :
      ('a -> 'b -> ('I, 'T, 'O, unit) m) ->
      'a list ->
      'b list ->
      ('I, 'T, 'O, unit) m

    (* *)

    val for_all :
      ('a -> ('I, 'T, 'O, bool) m) -> 'a list -> ('I, 'T, 'O, bool) m

    val exists : ('a -> ('I, 'T, 'O, bool) m) -> 'a list -> ('I, 'T, 'O, bool) m

    (* *)

    val traverse :
      ('a -> ('I, 'T, 'O, 'b) m) -> 'a list -> ('I, 'T, 'O, 'b list) m
  end

  module MOption : sig
    val iter : ('a -> ('I, 'T, 'O, unit) m) -> 'a option -> ('I, 'T, 'O, unit) m

    val traverse :
      ('a -> ('I, 'T, 'O, 'b) m) -> 'a option -> ('I, 'T, 'O, 'b option) m
  end
end

module Make (Core : Monad) = struct
  include Core

  let ( let+ ) aM ab =
    let* a = aM in
    return @@ ab a

  let ( >> ) xW yW =
    let* () = xW in
    yW

  let ( >>= ) = ( let* )
  let ( >>- ) = ( let+ )

  (* *)

  let lift1 xy x =
    let+ x = x in
    xy x

  let lift2 xyz x y =
    let* x = x in
    let+ y = y in
    xyz x y

  (* *)

  let ( &&& ) lhs rhs =
    let* lhs = lhs in
    if lhs then rhs else return false

  let ( ||| ) lhs rhs =
    let* lhs = lhs in
    if lhs then return true else rhs

  (* *)

  module MList = struct
    let rec fold_left xyx x = function
      | [] -> return x
      | y :: ys ->
        let* x = xyx x y in
        fold_left xyx x ys

    let rec fold_left2 xyzx x ys zs =
      match (ys, zs) with
      | [], [] -> return x
      | y :: ys, z :: zs ->
        let* x = xyzx x y z in
        fold_left2 xyzx x ys zs
      | _ -> raise @@ Invalid_argument "fold_left2"

    (* *)

    let iter yu = fold_left (Fun.const yu) ()
    let iter2 yzu = fold_left2 (Fun.const yzu) ()

    (* *)

    let rec for_all p = function
      | [] -> return true
      | x :: xs -> p x &&& for_all p xs

    let rec exists p = function
      | [] -> return false
      | x :: xs -> p x ||| exists p xs

    (* *)

    let traverse f xs =
      let* ys =
        xs
        |> fold_left
             (fun ys x ->
               let+ y = f x in
               y :: ys)
             []
      in
      return @@ List.rev ys
  end

  module MOption = struct
    let iter xuM = function None -> return () | Some x -> xuM x

    let traverse xyM = function
      | None -> return None
      | Some x ->
        let+ y = xyM x in
        Some y
  end
end
