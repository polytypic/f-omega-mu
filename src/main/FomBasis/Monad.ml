module type Monad = sig
  type ('t1, 'x) t

  val return : 'x -> ('e, 'x) t
  val ( let* ) : ('e, 'x) t -> ('x -> ('e, 'y) t) -> ('e, 'y) t
end

module type S = sig
  include Monad

  val ( >> ) : ('r, unit) t -> ('r, 'x) t -> ('r, 'x) t

  (* *)

  val lift1 : ('d1 -> 'c) -> ('e, 'd1) t -> ('e, 'c) t
  val lift2 : ('d1 -> 'd2 -> 'c) -> ('e, 'd1) t -> ('e, 'd2) t -> ('e, 'c) t

  (* *)

  val ( &&& ) : ('e, bool) t -> ('e, bool) t -> ('e, bool) t
  val ( ||| ) : ('e, bool) t -> ('e, bool) t -> ('e, bool) t

  (* *)

  val traverse : ('x -> ('e, 'y) t) -> 'x list -> ('e, 'y list) t
  val fold_left : ('y -> 'x -> ('e, 'y) t) -> 'y -> 'x list -> ('e, 'y) t
  val iter : ('x -> ('e, unit) t) -> 'x list -> ('e, unit) t
  val for_all : ('x -> ('e, bool) t) -> 'x list -> ('e, bool) t
  val exists : ('x -> ('e, bool) t) -> 'x list -> ('e, bool) t
end

module Make (Core : Monad) = struct
  include Core

  let ( >> ) xW yW =
    let* () = xW in
    yW

  (* *)

  let lift1 xy x =
    let* x = x in
    return @@ xy x

  let lift2 xyz x y =
    let* x = x in
    let* y = y in
    return @@ xyz x y

  (* *)

  let ( &&& ) lhs rhs =
    let* lhs = lhs in
    if lhs then rhs else return false

  let ( ||| ) lhs rhs =
    let* lhs = lhs in
    if lhs then return true else rhs

  (* *)

  let fold_left yxy y xs =
    let rec loop y = function
      | [] -> return y
      | x :: xs ->
        let* y = yxy y x in
        loop y xs
    in
    loop y xs

  let iter f = fold_left (Fun.const f) ()

  let traverse f xs =
    let* ys =
      xs
      |> fold_left
           (fun ys x ->
             let* y = f x in
             return (y :: ys))
           []
    in
    return @@ List.rev ys

  let rec for_all p = function
    | [] -> return true
    | x :: xs -> p x &&& for_all p xs

  let rec exists p = function
    | [] -> return false
    | x :: xs -> p x ||| exists p xs
end
