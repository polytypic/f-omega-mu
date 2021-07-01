open Rea

type 'v state = [`Empty of ([`Ok of 'v] -> unit) list | `Ok of 'v]
type 'v t = 'v state ref

let create v = ref @@ `Ok v
let empty = `Empty []

let take var _ =
  match !var with
  | `Ok _ as x ->
    var := empty;
    x
  | `Empty _ ->
    `Async
      (fun k ->
        match !var with
        | `Ok _ as x ->
          var := empty;
          k x
        | `Empty ks -> var := `Empty ((k :> [`Ok of 'v] -> unit) :: ks))

let fill var v =
  let ok = `Ok v in
  match !var with
  | `Empty [] -> var := ok
  | `Empty (k :: ks) ->
    var := `Empty ks;
    k ok
  | _ -> failwith "impossible"

let get var =
  let+ v = take var in
  fill var v;
  v

let mutate var fn =
  let+ v = take var in
  fill var (fn v)

let try_mutate var fn =
  let* v = take var in
  let* r = catch (fn v) in
  match r with
  | `Error e ->
    fill var v;
    fail e
  | `Ok v ->
    fill var v;
    unit

let try_modify var fn =
  let* v = take var in
  let* r = catch (fn v) in
  match r with
  | `Error e ->
    fill var v;
    fail e
  | `Ok (v, a) ->
    fill var v;
    return a
