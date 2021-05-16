type ('e, 'a) state = [`Empty of (('e, 'a) Res.t -> unit) list | ('e, 'a) Res.t]
type ('e, 'a) t = ('e, 'a) state ref

let empty () = ref @@ `Empty []

let get var _ =
  match !var with
  | (`Ok _ | `Error _) as x -> x
  | `Empty _ ->
    `Async
      (fun k ->
        match !var with
        | (`Ok _ | `Error _) as x -> k x
        | `Empty ks -> var := `Empty (k :: ks))

let put var res _ =
  match !var with
  | `Empty ks ->
    var := (res :> (_, _) state);
    ks |> List.iter (fun k -> k res);
    Rea.ok_unit
  | _ -> Rea.ok_unit
