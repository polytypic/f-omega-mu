type ('e, 'a) state =
  [ `Initial of unit -> unit
  | `Empty of (('e, 'a) Res.t -> unit) list
  | ('e, 'a) Res.t ]

type ('e, 'a) t = ('e, 'a) state ref

let create (op : (_, _, _) Rea.t) r =
  let open Rea in
  let var = ref (`Empty []) in
  var :=
    `Initial
      (fun () ->
        start r
          ( catch op >>- fun res ->
            match !var with
            | `Empty ks ->
              var := (res :> (_, _) state);
              ks |> List.iter (fun k -> k res)
            | _ -> failwith "impossible" ));
  `Ok var

let get var _ =
  match !var with
  | (`Ok _ | `Error _) as x -> x
  | `Initial _ ->
    `Async
      (fun k ->
        match !var with
        | (`Ok _ | `Error _) as x -> k x
        | `Empty ks -> var := `Empty (k :: ks)
        | `Initial ef ->
          var := `Empty [k];
          ef ())
  | `Empty _ ->
    `Async
      (fun k ->
        match !var with
        | (`Ok _ | `Error _) as x -> k x
        | `Empty ks -> var := `Empty (k :: ks)
        | _ -> failwith "impossible")
