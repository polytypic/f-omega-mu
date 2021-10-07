open Higher.Syntax
open Fun.Syntax
open Monad.Syntax

type ('e, 'a) cod = [('e, 'a) Res.t | `Async of (('e, 'a) Res.t -> unit) -> unit]
type ('r, 'e, 'a) t = 'r -> ('e, 'a) cod

let dispatch k = function (`Ok _ | `Error _) as x -> k x | `Async on -> on k
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

(* *)

let ok_unit = `Ok ()
let unit _ = ok_unit

(* *)

let start r (xM : ('r, Zero.t, unit) t) =
  match xM r with `Ok () -> () | `Error _ -> . | `Async on -> on ignore

(* *)

let fail e _ = `Error e

(* *)

let catch xM r =
  match xM r with
  | (`Ok _ | `Error _) as x -> `Ok x
  | `Async on -> `Async (fun k -> on @@ fun x -> k @@ `Ok x)

(* *)

include
  Higher.New'3
    (struct
      type nonrec ('r, 'e, 'a) t = ('r, 'e, 'a) t
    end)
    ()

type ('r, 'e, 'a) fr = ('r, 'e, f) app'2 Monad.t -> ('r, 'e, 'a, f) app'3

let methods =
  object
    method map : 'a 'b. ('a, 'b, _) Functor.map =
      fun xy xF -> inj (( let+ ) (prj xF) xy)

    method return : 'a. ('a, _) Applicative.return = fun x -> inj (return x)

    method pair : 'a 'b. ('a, 'b, _) Applicative.pair =
      fun xF yF -> inj (( and* ) (prj xF) (prj yF))

    method bind : 'a 'b. ('a, 'b, _) Monad.bind =
      fun xyF xF ->
        inj
          (let* x = prj xF in
           prj (xyF x))
  end

let run xF = xF methods |> prj

module Syntax = struct
  type ('r, 'e, 'a) rea = ('r, 'e, 'a) fr

  let start r uF = start r (run uF)

  (* *)

  let of_async op _ =
    inj @@ fun r ->
    `Async (fun k -> op r (fun e -> k @@ `Error e) (fun a -> k @@ `Ok a))

  let of_res x _ = inj @@ fun _ -> (x :> (_, _) cod)

  (* *)

  let fail e _ = fail e |> inj

  (* *)

  let try_in xyF eyF xF _ =
    inj @@ fun r ->
    match run xF r with
    | `Ok x -> run (xyF x) r
    | `Error e -> run (eyF e) r
    | `Async on ->
      `Async
        (fun k ->
          on @@ function
          | `Ok x -> run (xyF x) r |> dispatch k
          | `Error e -> run (eyF e) r |> dispatch k)

  let catch xF _ = catch (run xF) |> inj

  (* *)

  let map_error ef xF _ =
    inj @@ fun r ->
    match run xF r with
    | `Ok _ as a -> a
    | `Error e -> `Error (ef e)
    | `Async on ->
      `Async
        (fun k ->
          on @@ function `Ok _ as a -> k a | `Error e -> k @@ `Error (ef e))

  let generalize_error xF = map_error (function (_ : Zero.t) -> .) xF

  (* *)

  let env_as ra _ = inj @@ fun r -> `Ok (ra r)
  let with_env rs xF _ = inj @@ fun r -> run xF (rs r)
  let replace_env r = with_env (const r)

  (* *)

  let invoke raF _ = inj @@ fun r -> run (raF r) ()

  (* *)

  let get field = env_as @@ Field.get field
  let get_as field fn = env_as @@ fun r -> fn @@ Field.get field r
  let setting field v = with_env @@ Field.set field v
  let mapping field fn = with_env @@ Field.map field fn
end
