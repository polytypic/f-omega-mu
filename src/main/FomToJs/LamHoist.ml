open Rea
open StdlibPlus
open FomSource
open FomParser
open FomAST
open Lam

let constants_to_top inn =
  let cs = ref LamMap.empty in
  let add c =
    match LamMap.find_opt c !cs with
    | None ->
      let i =
        to_string c |> Lexer.coerce_to_id |> Var.of_string Loc.dummy
        |> Var.freshen
      in
      cs := LamMap.add c (i, LamMap.cardinal !cs, ref 1) !cs;
      `Var i
    | Some (i, _, n) ->
      n := !n + 1;
      `Var i
  in
  let rec analyze ~skip =
    let consider vs r =
      let+ may_hoist = if skip then return false else is_total r in
      match r with
      | _ when not may_hoist -> (vs, r)
      | `Const (`Keep _ | `Bool _ | `Nat _ | `Unit) | `Var _ -> (vs, r)
      | _ -> (vs, if VarSet.is_empty vs then add r else r)
    in
    function
    | `Const _ as r -> r |> consider VarSet.empty
    | `Var i as r -> r |> consider @@ VarSet.singleton i
    | `App (`Lam (i, e), x) ->
      let* evs, e = analyze ~skip:false e and* xvs, x = analyze ~skip:true x in
      `App (`Lam (i, e), x)
      |> consider @@ VarSet.union (VarSet.remove i evs) xvs
    | `App (`App (`Const c, x), y) when Const.is_bop c && Const.is_total c ->
      let* xvs, x = analyze ~skip:false x and* yvs, y = analyze ~skip:false y in
      `App (`App (`Const c, x), y) |> consider @@ VarSet.union xvs yvs
    | `App (`Const c, x) when Const.is_uop c && Const.is_total c ->
      let* vs, x = analyze ~skip:false x in
      `App (`Const c, x) |> consider vs
    | `App (f, x) ->
      let* fvs, f = analyze ~skip:false f and* xvs, x = analyze ~skip:false x in
      `App (f, x) |> consider @@ VarSet.union fvs xvs
    | `IfElse (c, t, e) ->
      let* cvs, c = analyze ~skip:false c
      and* tvs, t = analyze ~skip:false t
      and* evs, e = analyze ~skip:false e in
      `IfElse (c, t, e) |> consider @@ VarSet.union cvs (VarSet.union tvs evs)
    | `Product fs ->
      let* vs, fs = analyze_product ~skip:false fs in
      `Product fs |> consider vs
    | `Mu (`Lam (f, e)) ->
      let* vs, e = analyze ~skip:true e in
      `Mu (`Lam (f, e)) |> consider @@ VarSet.remove f vs
    | `Mu (`Case (`Product fs)) ->
      let* vs, fs = analyze_product ~skip:true fs in
      `Mu (`Case (`Product fs)) |> consider vs
    | `Mu e ->
      let* vs, e = analyze ~skip:false e in
      `Mu e |> consider vs
    | `Lam (i, e) ->
      let* vs, e = analyze ~skip:false e in
      `Lam (i, e) |> consider @@ VarSet.remove i vs
    | `Inject (l, e) ->
      let* vs, e = analyze ~skip:false e in
      `Inject (l, e) |> consider vs
    | `Select (e, `Inject (l, `Const `Unit)) ->
      let* vs, e = analyze ~skip:false e in
      `Select (e, `Inject (l, `Const `Unit)) |> consider vs
    | `Select (e, l) ->
      let* evs, e = analyze ~skip:false e and* lvs, l = analyze ~skip:false l in
      `Select (e, l) |> consider @@ VarSet.union evs lvs
    | `Case (`Product fs) ->
      let* vs, fs = analyze_product ~skip:true fs in
      `Case (`Product fs) |> consider vs
    | `Case cs ->
      let* vs, cs = analyze ~skip:false cs in
      `Case cs |> consider vs
  and analyze_product ~skip fs =
    let+ fs = fs |> Row.map_er (analyze ~skip) in
    ( fs |> List.fold_left (fun s (_, (vs, _)) -> VarSet.union s vs) VarSet.empty,
      fs |> Row.map snd )
  in
  let+ _, e = analyze ~skip:true inn in
  !cs |> LamMap.bindings
  |> List.sort (fun (_, (_, l, _)) (_, (_, r, _)) -> Int.compare r l)
  |> List.fold_left (fun e (v, (i, _, _)) -> `App (`Lam (i, e), v)) e
