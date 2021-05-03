include Monad.Make (struct
  type ('r, 'e, 'a) t =
    'r ->
    [`Ok of 'a | `Error of 'e | `Async of (('a, 'e) result -> unit) -> unit]

  let return x _ = `Ok x

  let ( let* ) xM xyM r =
    match xM r with
    | `Ok x -> xyM x r
    | `Error e -> `Error e
    | `Async on ->
      `Async
        (fun k ->
          on @@ function
          | Ok x -> (
            match xyM x r with
            | `Ok y -> k @@ Ok y
            | `Error e -> k @@ Error e
            | `Async on -> on k)
          | Error e -> k @@ Error e)

  let ( let+ ) xM xy r =
    match xM r with
    | `Ok x -> `Ok (xy x)
    | `Error e -> `Error e
    | `Async on ->
      `Async
        (fun k ->
          on @@ function Ok x -> k @@ Ok (xy x) | Error e -> k @@ Error e)
end)

let start r (xM : ('r, Zero.t, unit) t) =
  match xM r with `Ok () -> () | `Error _ -> . | `Async on -> on ignore

(* *)

let fail e _ = `Error e

(* *)

let map_either ef ab xM r =
  match xM r with
  | `Ok a -> `Ok (ab a)
  | `Error e -> `Error (ef e)
  | `Async on ->
    `Async
      (fun k ->
        on @@ function Ok a -> k @@ Ok (ab a) | Error e -> k @@ Error (ef e))

let map_error ef = map_either ef Fun.id

(* *)

let try_in xM xyM eyM r =
  match xM r with
  | `Ok x -> xyM x r
  | `Error e -> eyM e r
  | `Async on ->
    `Async
      (fun k ->
        on @@ function
        | Ok x -> (
          match xyM x r with
          | `Ok y -> k @@ Ok y
          | `Error e -> k @@ Error e
          | `Async on -> on k)
        | Error e -> (
          match eyM e r with
          | `Ok y -> k @@ Ok y
          | `Error e -> k @@ Error e
          | `Async on -> on k))

(* *)

let env_as ra r = `Ok (ra r)
let with_env rs xM r = xM (rs r)

(* *)

let get field = env_as (Field.get field)
let get_as field fn = env_as (fun r -> fn @@ Field.get field r)
let setting field v = with_env (Field.set field v)
let mapping field fn = with_env (Field.map field fn)
