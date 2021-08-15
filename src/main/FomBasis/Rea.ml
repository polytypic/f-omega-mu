type ('e, 'a) cod = [('e, 'a) Res.t | `Async of (('e, 'a) Res.t -> unit) -> unit]
type ('r, 'e, 'a) t = 'r -> ('e, 'a) cod

let dispatch k = function (`Ok _ | `Error _) as x -> k x | `Async on -> on k

include Monad.Make (struct
  type ('r, 'T, 'e, 'a) m = ('r, 'e, 'a) t

  let return x _ = `Ok x

  let ( let* ) xM xyM r =
    match xM r with
    | `Ok x -> xyM x r
    | `Error _ as e -> e
    | `Async on ->
      `Async
        (fun k ->
          on @@ function `Ok x -> xyM x r |> dispatch k | `Error _ as e -> k e)

  let ( let+ ) xM xy r =
    match xM r with
    | `Ok x -> `Ok (xy x)
    | `Error _ as e -> e
    | `Async on ->
      `Async
        (fun k ->
          on @@ function `Ok x -> k @@ `Ok (xy x) | `Error _ as e -> k e)

  let pairing x k = function `Ok y -> k @@ `Ok (x, y) | `Error _ as e -> k e

  let ( and* ) xM yM r =
    match xM r with
    | `Ok x -> (
      match yM r with
      | `Ok y -> `Ok (x, y)
      | `Error _ as e -> e
      | `Async on -> `Async (fun k -> on @@ pairing x k))
    | `Error _ as e -> e
    | `Async on ->
      `Async
        (fun k ->
          on @@ function
          | `Ok x -> (
            match yM r with
            | `Ok y -> k @@ `Ok (x, y)
            | `Error _ as e -> k e
            | `Async on -> on @@ pairing x k)
          | `Error _ as e -> k e)
end)

(* *)

let ok_unit = `Ok ()
let unit _ = ok_unit
let delay urea r = urea () r

(* *)

let start r (xM : ('r, Zero.t, unit) t) =
  match xM r with `Ok () -> () | `Error _ -> . | `Async on -> on ignore

(* *)

let of_async op r =
  `Async (fun k -> op r (fun e -> k @@ `Error e) (fun a -> k @@ `Ok a))

let of_res x _ = (x :> (_, _) cod)

(* *)

let fail e _ = `Error e

(* *)

let map_error ef xM r =
  match xM r with
  | `Ok _ as a -> a
  | `Error e -> `Error (ef e)
  | `Async on ->
    `Async
      (fun k ->
        on @@ function `Ok _ as a -> k a | `Error e -> k @@ `Error (ef e))

let generalize_error (xM : ('r, Zero.t, 'a) t) =
  map_error (fun _ -> failwith "impossible") xM

(* *)

let try_in xyM eyM xM r =
  match xM r with
  | `Ok x -> xyM x r
  | `Error e -> eyM e r
  | `Async on ->
    `Async
      (fun k ->
        on @@ function
        | `Ok x -> xyM x r |> dispatch k
        | `Error e -> eyM e r |> dispatch k)

let catch xM r =
  match xM r with
  | (`Ok _ | `Error _) as x -> `Ok x
  | `Async on -> `Async (fun k -> on @@ fun x -> k @@ `Ok x)

(* *)

let env_as ra r = `Ok (ra r)
let with_env rs xM r = xM (rs r)
let replace_env r = with_env (Fun.const r)

(* *)

let invoke raM r = raM r ()

(* *)

let get field = env_as @@ Field.get field
let get_as field fn = env_as @@ fun r -> fn @@ Field.get field r
let setting field v = with_env @@ Field.set field v
let mapping field fn = with_env @@ Field.map field fn
