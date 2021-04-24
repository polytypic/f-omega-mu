open FomBasis
open FomAnnot
open FomDiag

(* *)

include FomAST.Exp

(* *)

let typ_infer_and_norm typ =
  let open Reader in
  let* kind = Typ.infer typ in
  match kind with
  | `Star _ -> return @@ Typ.norm typ
  | _ -> Error.typ_of_kind_arrow (Typ.at typ) typ kind

let check_arrow_typ at typ =
  match Typ.unfold_of_norm typ with
  | `Arrow (_, dom, cod) -> (dom, cod)
  | _ -> Error.typ_unexpected at "_ → _" typ

let check_product_typ at typ =
  match Typ.unfold_of_norm typ with
  | `Product (_, ls) -> ls
  | _ -> Error.typ_unexpected at "{_}" typ

let check_sum_typ at typ =
  match Typ.unfold_of_norm typ with
  | `Sum (_, ls) -> ls
  | _ -> Error.typ_unexpected at "[_]" typ

let check_for_all_typ at typ =
  let open Reader in
  match Typ.unfold_of_norm typ with
  | `ForAll (_, f_con) -> (
    let* f_kind = Typ.kind_of f_con in
    match f_kind with
    | `Arrow (_, d_kind, `Star _) -> return @@ (f_con, d_kind)
    | _ -> failwith "impossible")
  | _ -> Error.typ_unexpected at "∀(_)" typ

let check_exists_typ at typ =
  let open Reader in
  match Typ.unfold_of_norm typ with
  | `Exists (_, f_con) -> (
    let* f_kind = Typ.kind_of f_con in
    match f_kind with
    | `Arrow (_, d_kind, `Star _) -> return @@ (f_con, d_kind)
    | _ -> failwith "impossible")
  | _ -> Error.typ_unexpected at "∃(_)" typ

let rec infer it : (_, _, Typ.t) Reader.t =
  let open Reader in
  match it with
  | `Const (at, c) -> typ_infer_and_norm (Const.type_of at c)
  | `Var (at, x) -> (
    let* x_typ_opt = env_as @@ fun e -> Env.find_opt x e#get_exp_env in
    match x_typ_opt with
    | None -> Error.var_unbound at x
    | Some (def, x_typ) -> env_as (Annot.Exp.use x (Id.at def)) >> return x_typ)
  | `Lam (at, d, d_typ, r) ->
    let* d_typ = typ_infer_and_norm d_typ in
    env_as (Annot.Exp.def d d_typ)
    >> let* r_typ =
         infer r |> with_env @@ fun e -> Env.add d (d, d_typ) |> e#map_exp_env
       in
       return @@ `Arrow (at, d_typ, r_typ)
  | `App (_, f, x) ->
    let* f_typ = infer f in
    let d_typ, c_typ = check_arrow_typ (at f) f_typ in
    let* x_typ = infer x in
    Typ.check_sub_of_norm (at x) (x_typ, d_typ);
    return c_typ
  | `Gen (at, d, d_kind, r) ->
    env_as (Annot.Typ.def d d_kind)
    >> let* r_typ =
         infer r
         |> with_env @@ fun e -> Typ.Env.add d (d, d_kind) |> e#map_typ_env
       in
       return @@ `ForAll (at, Typ.norm (`Lam (at, d, d_kind, r_typ)))
  | `Inst (at', f, x_typ) ->
    let* f_typ = infer f in
    let* f_con, d_kind = check_for_all_typ at' f_typ in
    let* x_kind = Typ.infer x_typ in
    Kind.check_equal at' d_kind x_kind;
    return @@ Typ.norm (`App (at', f_con, x_typ))
  | `LetIn (_, d, x, r) ->
    let* x_typ = infer x in
    env_as (Annot.Exp.def d x_typ)
    >> infer r
    |> with_env @@ fun e -> Env.add d (d, x_typ) |> e#map_exp_env
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
      |> traverse @@ fun (l, e) ->
         let* e_typ = infer e in
         return (l, e_typ)
    in
    return @@ Typ.product at fs
  | `Select (_, p, l) -> (
    let* p_typ = infer p in
    match
      check_product_typ (at p) p_typ |> List.find_opt (fst >>> Label.equal l)
    with
    | Some (l', l_typ) ->
      env_as (Annot.Label.def l' l_typ)
      >> env_as (Annot.Label.use l (Label.at l'))
      >> return l_typ
    | None -> Error.product_lacks (at p) p_typ l)
  | `Inject (at, l, e) ->
    let* e_typ = infer e in
    return @@ Typ.sum at [(l, e_typ)]
  | `Case (at', cs) ->
    let* cs_typ = infer cs in
    let cs_fs = check_product_typ (at cs) cs_typ in
    let cs_arrows =
      cs_fs |> List.map (Pair.map Fun.id (check_arrow_typ (at cs)))
    in
    let c_typ =
      match cs_arrows |> List.map (snd >>> snd) with
      | [] -> Typ.zero (at cs)
      | t :: ts ->
        ts |> List.fold_left (fun a t -> Typ.join_of_norm (at cs) (a, t)) t
    in
    let d_typ = `Sum (at cs, cs_arrows |> List.map (Pair.map Fun.id fst)) in
    return @@ `Arrow (at', d_typ, c_typ)
  | `Pack (at', t, e, et) ->
    let* e_typ = infer e in
    let* t_kind = Typ.infer t in
    let* et = typ_infer_and_norm et in
    let* et_con, d_kind = check_exists_typ at' et in
    Kind.check_equal at' d_kind t_kind;
    let et_t = Typ.norm (`App (at', et_con, t)) in
    Typ.check_sub_of_norm (at e) (e_typ, et_t);
    return et
  | `UnpackIn (at, tid, id, v, e) ->
    let* v_typ = infer v in
    let* v_con, d_kind = check_exists_typ at v_typ in
    let id_typ = Typ.norm (`App (at, v_con, `Var (at, tid))) in
    env_as (Annot.Exp.def id id_typ)
    >> env_as (Annot.Typ.def tid d_kind)
    >> let* e_typ =
         infer e
         |> with_env @@ fun r ->
            let r = r#map_typ_env (Typ.Env.add tid (tid, d_kind)) in
            let r = r#map_exp_env (Env.add id (id, id_typ)) in
            r
       in
       if Typ.is_free tid e_typ then
         Error.typ_var_escapes at tid e_typ;
       return e_typ
  | `Target (_, t, _) -> typ_infer_and_norm t
