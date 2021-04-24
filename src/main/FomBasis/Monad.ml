module type Monad = sig
  type ('T1, 'T2, 'a) t

  val return : 'a -> ('T1, 'T2, 'a) t

  val ( let* ) :
    ('T1, 'T2, 'a) t -> ('a -> ('T1, 'T2, 'b) t) -> ('T1, 'T2, 'b) t
end

module type S = sig
  include Monad

  val ( >> ) : ('T1, 'T2, unit) t -> ('T1, 'T2, 'a) t -> ('T1, 'T2, 'a) t

  (* *)

  val lift1 : ('d1 -> 'c) -> ('T1, 'T2, 'd1) t -> ('T1, 'T2, 'c) t

  val lift2 :
    ('d1 -> 'd2 -> 'c) ->
    ('T1, 'T2, 'd1) t ->
    ('T1, 'T2, 'd2) t ->
    ('T1, 'T2, 'c) t

  (* *)

  val ( &&& ) : ('T1, 'T2, bool) t -> ('T1, 'T2, bool) t -> ('T1, 'T2, bool) t
  val ( ||| ) : ('T1, 'T2, bool) t -> ('T1, 'T2, bool) t -> ('T1, 'T2, bool) t

  (* *)

  val traverse : ('a -> ('T1, 'T2, 'b) t) -> 'a list -> ('T1, 'T2, 'b list) t

  val fold_left :
    ('b -> 'a -> ('T1, 'T2, 'b) t) -> 'b -> 'a list -> ('T1, 'T2, 'b) t

  val iter : ('a -> ('T1, 'T2, unit) t) -> 'a list -> ('T1, 'T2, unit) t
  val for_all : ('a -> ('T1, 'T2, bool) t) -> 'a list -> ('T1, 'T2, bool) t
  val exists : ('a -> ('T1, 'T2, bool) t) -> 'a list -> ('T1, 'T2, bool) t
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
