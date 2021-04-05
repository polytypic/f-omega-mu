open FomBasis
open FomAnnot
open FomDiag

(* *)

include FomAST.Exp

(* *)

let typ_check_and_norm typ =
  let open Reader in
  let* kind = Typ.infer typ in
  match kind with
  | `Star _ -> return @@ Typ.norm typ
  | _ -> Error.typ_of_kind_arrow (Typ.at typ) typ kind

let check_arrow_typ at typ =
  match Typ.unfold_of_norm typ with
  | `Arrow (_, dom, cod) -> (dom, cod)
  | _ -> Error.typ_non_arrow at typ

let check_product_typ at typ =
  match Typ.unfold_of_norm typ with
  | `Product (_, ls) -> ls
  | _ -> Error.typ_non_product at typ

let check_sum_typ at typ =
  match Typ.unfold_of_norm typ with
  | `Sum (_, ls) -> ls
  | _ -> Error.typ_non_sum at typ

let rec infer_and_unfold e =
  let open Reader in
  let* e_typ = infer e in
  return @@ Typ.unfold_of_norm e_typ

and infer it : _ -> Typ.t =
  let open Reader in
  match it with
  | `Const (at, c) -> typ_check_and_norm (Const.type_of at c)
  | `Var (at, x) -> (
    let* x_typ_opt e = Env.find_opt x e#get_exp_env in
    match x_typ_opt with
    | None -> Error.var_unbound at x
    | Some (def, x_typ) ->
      let* () = Annot.Exp.use x (Id.at def) in
      return x_typ)
  | `Lam (at, d, d_typ, r) ->
    let* d_typ = typ_check_and_norm d_typ in
    let* () = Annot.Exp.def d d_typ in
    let* r_typ e = Env.add d (d, d_typ) |> e#map_exp_env |> infer r in
    return @@ `Arrow (at, d_typ, r_typ)
  | `App (_, f, x) ->
    let* f_typ = infer f in
    let d_typ, c_typ = check_arrow_typ (at f) f_typ in
    let* x_typ = infer x in
    Typ.check_sub_of_norm (at x) (x_typ, d_typ);
    return c_typ
  | `Gen (at, d, d_kind, r) ->
    let* () = Annot.Typ.def d d_kind in
    let* r_typ e = Typ.Env.add d (d, d_kind) |> e#map_typ_env |> infer r in
    return @@ `ForAll (at, Typ.norm (`Lam (at, d, d_kind, r_typ)))
  | `Inst (at, f, x_typ) -> (
    let* f_typ = infer_and_unfold f in
    match f_typ with
    | `ForAll (_, f_con) -> (
      let* f_kind = Typ.kind_of f_con in
      match f_kind with
      | `Arrow (_, d_kind, `Star _) ->
        let* x_kind = Typ.infer x_typ in
        Kind.check_equal at d_kind x_kind;
        return @@ Typ.norm (`App (at, f_con, x_typ))
      | _ -> failwith "Impossible")
    | _ -> Error.inst_of_non_for_all at f f_typ)
  | `LetIn (_, d, x, r) ->
    let* x_typ = infer x in
    let* () = Annot.Exp.def d x_typ in
    fun e -> Env.add d (d, x_typ) |> e#map_exp_env |> infer r
  | `Mu (at', f) ->
    let* f_typ = infer f in
    let d_typ, c_typ = check_arrow_typ (at f) f_typ in
    Typ.check_sub_of_norm at' (c_typ, d_typ);
    return c_typ
  | `IfElse (_, c, t, e) ->
    let* c_typ = infer c in
    Typ.check_sub_of_norm (at c) (c_typ, `Const (at c, `Bool));
    let* t_typ = infer t in
    let* e_typ = infer e in
    return @@ Typ.join_of_norm (at e) (t_typ, e_typ)
  | `Product (at, fs) ->
    let* fs =
      fs
      |> traverse (fun (l, e) ->
             let* e_typ = infer e in
             return (l, e_typ))
    in
    return @@ Typ.product at fs
  | `Select (_, p, l) -> (
    let* p_typ = infer p in
    match
      check_product_typ (at p) p_typ
      |> List.find_opt (fun (l', _) -> Label.equal l' l)
    with
    | Some (l', l_typ) ->
      let* () = Annot.Label.def l' l_typ in
      let* () = Annot.Label.use l (Label.at l') in
      return l_typ
    | None -> Error.product_lacks (at p) p_typ l)
  | `Inject (at, l, e) ->
    let* e_typ = infer e in
    return @@ Typ.sum at [(l, e_typ)]
  | `Case (at', cs) ->
    let* cs_typ = infer cs in
    let cs_fs = check_product_typ (at cs) cs_typ in
    let cs_arrows = cs_fs |> List.map (Pair.map id (check_arrow_typ (at cs))) in
    let c_typ =
      match cs_arrows |> List.map (snd >> snd) with
      | [] -> Typ.zero (at cs)
      | t :: ts ->
        ts |> List.fold_left (fun a t -> Typ.join_of_norm (at cs) (a, t)) t
    in
    let d_typ = `Sum (at cs, cs_arrows |> List.map (Pair.map id fst)) in
    return @@ `Arrow (at', d_typ, c_typ)
  | `Pack (at', t, e, et) -> (
    let* e_typ = infer e in
    let* t_kind = Typ.infer t in
    let* et = typ_check_and_norm et in
    match Typ.unfold_of_norm et with
    | `Exists (_, et_con) -> (
      let* et_kind = Typ.kind_of et_con in
      match et_kind with
      | `Arrow (_, d_kind, `Star _) ->
        Kind.check_equal at' d_kind t_kind;
        let et_t = Typ.norm (`App (at', et_con, t)) in
        Typ.check_sub_of_norm (at e) (e_typ, et_t);
        return et
      | _ -> failwith "Impossible")
    | _ -> failwith "TODO: pack non existential")
  | `UnpackIn (at, tid, id, v, e) -> (
    let* v_typ = infer_and_unfold v in
    match v_typ with
    | `Exists (_, v_con) -> (
      let* f_kind = Typ.kind_of v_con in
      match f_kind with
      | `Arrow (_, d_kind, `Star _) ->
        let id_typ = Typ.norm (`App (at, v_con, `Var (at, tid))) in
        let* () = Annot.Exp.def id id_typ in
        let* () = Annot.Typ.def tid d_kind in
        let* e_typ r =
          let r = r#map_typ_env (Typ.Env.add tid (tid, d_kind)) in
          let r = r#map_exp_env (Env.add id (id, id_typ)) in
          infer e r
        in
        if Typ.is_free tid e_typ then
          failwith "TODO: Type variable escapes";
        return e_typ
      | _ -> failwith "Impossible")
    | _ -> failwith "TODO: unpack non-existential")
  | `Target (_, t, _) -> typ_check_and_norm t
