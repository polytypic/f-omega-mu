open FomBasis
open FomSource
open FomAST
open Lam

let move_cses_up inn =
  let counter = ref 0 in
  let index () =
    let i = !counter + 1 in
    counter := i;
    i
  in
  let intersect _ l r =
    match (l, r) with
    | Some (ln, li), Some (rn, ri) -> Some (ln + rn, min li ri)
    | _ -> None
  in
  let union _ l r =
    match (l, r) with
    | Some x, None | None, Some x -> Some x
    | Some (ln, li), Some (rn, ri) -> Some (ln + rn, min li ri)
    | None, None -> None
  in
  let rec move_cses_up = function
    | `Lam (i, e) ->
      let+ e, cses = move_cses_up e in
      let e =
        cses |> LamMap.bindings
        |> List.filter (fun (e, _) -> is_free i e)
        |> List.sort (Compare.the (snd >>> snd) Int.compare)
        |> List.rev
        |> List.fold_left
             (fun e (cse, (n, _)) ->
               if 1 < n then (
                 let i = Var.fresh Loc.dummy in
                 let v = `Var i in
                 let e' = replace cse v e in
                 Printf.printf "Replacing %s\nin %s\nto %s\n"
                   (Lam.pp cse |> FomPP.to_string)
                   (Lam.pp e |> FomPP.to_string)
                   (Lam.pp e' |> FomPP.to_string);
                 `App (`Lam (i, e'), cse))
               else
                 e)
             e
      in
      let exp = `Lam (i, e) in
      let cses = cses |> LamMap.filter (fun e _ -> not (is_free i e)) in
      (exp, cses)
    | `App (f, x) ->
      let+ f, f_cses = move_cses_up f and+ x, x_cses = move_cses_up x in
      let exp = `App (f, x) in
      ( exp,
        LamMap.merge union f_cses x_cses
        |> LamMap.merge union @@ LamMap.singleton exp (1, index ()) )
    | `Case (`Product fs) ->
      let+ fs_cses = fs |> Row.map_fr move_cses_up in
      let exp =
        `Case (`Product (fs_cses |> List.map (fun (l, (e, _)) -> (l, e))))
      in
      let cses =
        fs_cses
        |> List.fold_left
             (fun cses (_, (_, e_cses)) -> LamMap.merge intersect cses e_cses)
             LamMap.empty
        |> LamMap.merge union (LamMap.singleton exp (1, index ()))
      in
      (exp, cses)
    | `Case e ->
      let+ e, e_cses = move_cses_up e in
      let exp = `Case e in
      (exp, LamMap.merge union e_cses @@ LamMap.singleton exp (1, index ()))
    | `IfElse (c, t, e) ->
      let+ c, c_cses = move_cses_up c
      and+ t, t_cses = move_cses_up t
      and+ e, e_cses = move_cses_up e in
      let exp = `IfElse (c, t, e) in
      let cses =
        (* TODO: does not include all *)
        LamMap.merge intersect t_cses e_cses
        |> LamMap.merge union
           @@ LamMap.merge intersect c_cses
           @@ LamMap.merge union t_cses e_cses
      in
      (exp, cses)
    | `Inject (l, e) ->
      let+ e, e_cses = move_cses_up e in
      let exp = `Inject (l, e) in
      (exp, LamMap.merge union e_cses @@ LamMap.singleton exp (1, index ()))
    | `Mu e ->
      let+ e, e_cses = move_cses_up e in
      let exp = `Mu e in
      (exp, LamMap.merge union e_cses @@ LamMap.singleton exp (1, index ()))
    | `Product fs ->
      let+ fs_cses = fs |> Row.map_fr move_cses_up in
      let exp = `Product (fs_cses |> List.map (fun (l, (e, _)) -> (l, e))) in
      let cses =
        fs_cses
        |> List.fold_left
             (fun cses (_, (_, e_cses)) -> LamMap.merge union cses e_cses)
             (LamMap.singleton exp (1, index ()))
      in
      (exp, cses)
    | `Select (e, l) ->
      let+ e, e_cses = move_cses_up e and+ l, l_cses = move_cses_up l in
      let exp = `Select (e, l) in
      ( exp,
        LamMap.merge union e_cses l_cses
        |> LamMap.merge union @@ LamMap.singleton exp (1, index ()) )
    | (`Const _ | `Var _) as exp -> return (exp, LamMap.empty)
  in
  let+ e, _ = move_cses_up inn in
  e
