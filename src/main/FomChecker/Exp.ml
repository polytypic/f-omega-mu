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

let check_typs_sub at ~sub ~sup =
  if not (Typ.sub_of_norm sub sup) then
    Error.typ_mismatch at sub sup

let check_typs_equal at ~exp ~act =
  if not (Typ.equal_of_norm exp act) then
    Error.typ_mismatch at exp act

let check_typs_join at lhs rhs =
  match Typ.join_of_norm lhs rhs with
  | None -> Error.typ_mismatch at lhs rhs
  | Some typ -> typ

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
    check_typs_sub (at x) ~sub:x_typ ~sup:d_typ;
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
        if not (Kind.equal d_kind x_kind) then
          Error.inst_kind_mismatch at f_typ d_kind x_typ x_kind;
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
    check_typs_sub at' ~sub:c_typ ~sup:d_typ;
    return c_typ
  | `IfElse (_, c, t, e) ->
    let* c_typ = infer c in
    check_typs_equal (at c) ~exp:(`Const (at c, `Bool)) ~act:c_typ;
    let* t_typ = infer t in
    let* e_typ = infer e in
    return @@ check_typs_join (at e) t_typ e_typ
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
  | `Case (at', s, cs) -> (
    let* s_typ = infer s in
    let s_fs = check_sum_typ (at s) s_typ in
    let* cs_typ = infer cs in
    let cs_fs = check_product_typ (at cs) cs_typ in
    if ListExt.compare_with (Compare.the fst Label.compare) s_fs cs_fs <> 0 then
      Error.labels_mismatch at' (List.map fst s_fs) (List.map fst cs_fs);
    match
      cs_fs |> List.map (Pair.map id @@ fun t -> check_arrow_typ (Typ.at t) t)
    with
    | [] -> return s_typ
    | (_, (_, e_cod)) :: _ as cs_fs ->
      List.combine cs_fs s_fs
      |> fold_left
           (fun e_cod ((l, (c_dom, c_cod)), (l', e_typ)) ->
             check_typs_sub (Typ.at c_dom) ~sub:e_typ ~sup:c_dom;
             let* () = Annot.Label.def l' e_typ in
             let* () = Annot.Label.use l (Label.at l') in
             return @@ check_typs_join (Typ.at c_cod) c_cod e_cod)
           e_cod)
  | `Pack (at', t, e, et) -> (
    let* e_typ = infer e in
    let* t_kind = Typ.infer t in
    let* et = typ_check_and_norm et in
    match Typ.unfold_of_norm et with
    | `Exists (_, et_con) -> (
      let* et_kind = Typ.kind_of et_con in
      match et_kind with
      | `Arrow (_, d_kind, `Star _) ->
        if not (Kind.equal d_kind t_kind) then
          failwith "TODO: kind mismatch";
        let et_t = Typ.norm (`App (at', et_con, t)) in
        check_typs_sub (at e) ~sub:e_typ ~sup:et_t;
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
