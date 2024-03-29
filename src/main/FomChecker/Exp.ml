open Rea
open StdlibPlus
open FomSource
open FomAnnot

(* *)

include FomAST.Exp

(* *)

module VarEnv = struct
  type m = (Var.t * Typ.Core.t) VarMap.t Prop.t

  let field r : m = r#exp_env
  let adding v t = mapping field @@ VarMap.add v (v, t)
  let find_opt i = get_as field @@ VarMap.find_opt i

  class con =
    object
      val mutable exp_env = VarMap.empty
      method exp_env : m = prop (fun () -> exp_env) (fun x -> exp_env <- x)
    end
end

(* *)

module Typ = struct
  include Typ

  let check_and_norm typ = check_and_resolve (`Star (at typ)) typ

  let check_arrow at typ =
    Core.unfold_of_norm typ >>= function
    | `Arrow (_, dom, cod) -> pure (dom, cod)
    | _ -> fail @@ `Error_typ_unexpected (at, "_ → _", typ)

  let check_product at typ =
    Core.unfold_of_norm typ >>= function
    | `Row (_, `Product, ls) -> pure ls
    | _ -> fail @@ `Error_typ_unexpected (at, "{_}", typ)

  let check_sum at typ =
    Core.unfold_of_norm typ >>= function
    | `Row (_, `Sum, ls) -> pure ls
    | _ -> fail @@ `Error_typ_unexpected (at, "'_", typ)

  let check_unit at typ =
    Core.unfold_of_norm typ >>= function
    | `Const (_, `Unit) -> unit
    | _ -> fail @@ `Error_typ_unexpected (at, "()", typ)

  let check_atom at typ =
    let* ls = check_sum at typ in
    match ls with
    | [] -> fail @@ `Error_typ_unexpected (at, "'_", typ)
    | ls -> ls |> List.iter_er (snd >>> check_unit at) >> pure'2 List.map fst ls

  let check_for_all at typ =
    Core.unfold_of_norm typ >>= function
    | `For (_, `All, f_con) -> (
      let* f_kind = kind_of f_con in
      match f_kind with
      | `Arrow (_, d_kind, `Star _) -> pure (f_con, d_kind)
      | _ -> failwith "check_for_all")
    | _ -> fail @@ `Error_typ_unexpected (at, "∀(_)", typ)

  let check_exists at typ =
    Core.unfold_of_norm typ >>= function
    | `For (_, `Unk, f_con) -> (
      let* f_kind = kind_of f_con in
      match f_kind with
      | `Arrow (_, d_kind, `Star _) -> pure (f_con, d_kind)
      | _ -> failwith "check_exists")
    | _ -> fail @@ `Error_typ_unexpected (at, "∃(_)", typ)
end

let cannot_be_for_all = function
  | `Case _ | `Lam _ | `LamImp _ | `Inject _ | `Merge _ | `Pack _ | `PackImp _
  | `Product _ ->
    true
  | `Const (at', c) -> (
    match Const.type_of at' c with `For (_, `All, _) -> false | _ -> true)
  | _ -> false

let rec infer e =
  eta'0 @@ fun () ->
  match e with
  | `App (at', f, x) -> (
    match f with
    | `Lam (at'', i, t, e) ->
      let* t = Typ.check_and_norm t in
      let+ x = check t x
      and+ e, e_typ = Annot.Exp.def i t >> VarEnv.adding i t (infer e) in
      (`App (at', `Lam (at'', i, t, e), x), e_typ)
    | `LamImp (_, d, r) ->
      let* x, x_typ = infer x in
      let+ r, r_typ =
        Annot.Exp.def d x_typ >> VarEnv.adding d x_typ (infer r)
      in
      (`App (at', `Lam (at', d, x_typ, r), x), r_typ)
    | f ->
      let* f, f_typ = infer f in
      let* d_typ, c_typ = Typ.check_arrow (at f) f_typ in
      let+ x = check d_typ x in
      (`App (at', f, x), c_typ))
  | `Const (at', c) ->
    let+ c = Const.map_typ_er Typ.check_and_norm c in
    (`Const (at', c), Const.type_of at' c)
  | `Var (at', x) as e -> (
    let* x_typ_opt = VarEnv.find_opt x in
    match x_typ_opt with
    | None -> fail @@ `Error_var_unbound (at', x)
    | Some (def, x_typ) -> Annot.Exp.use x (Var.at def) >> pure (e, x_typ))
  | `Lam (at', d, d_typ, r) ->
    let* d_typ = Typ.check_and_norm d_typ in
    let+ r, r_typ = Annot.Exp.def d d_typ >> VarEnv.adding d d_typ (infer r) in
    (`Lam (at', d, d_typ, r), `Arrow (at', d_typ, r_typ))
  | `LamImp (_, i, _) -> fail @@ `Error_pat_lacks_annot (Var.at i)
  | `Case (at', cs) ->
    let* cs, cs_typ = infer cs in
    let* cs_fs = Typ.check_product (at cs) cs_typ in
    let* cs_arrows = Row.map_er (Typ.check_arrow (at cs)) cs_fs in
    let+ c_typ =
      match cs_arrows |> List.map (snd >>> snd) with
      | [] -> pure'1 Typ.zero (at cs)
      | t :: ts -> ts |> List.fold_left_er (Typ.join_of_norm (at cs)) t
    in
    let d_typ = Typ.sum (at cs) (Row.map fst cs_arrows) in
    (`Case (at', cs), `Arrow (at', d_typ, c_typ))
  | `Gen (at', d, d_kind, r) ->
    let* r, r_typ = infer r |> Typ.VarEnv.adding d @@ `Kind d_kind in
    let* d_kind = Kind.resolve d_kind >>- Kind.ground in
    let+ f = Typ.Core.lam_of_norm at' d d_kind r_typ in
    (`Gen (at', d, d_kind, r), `For (at', `All, f))
  | `Inst (at', f, x_typ) ->
    let* f, f_typ = infer f in
    let* f_con, d_kind = Typ.check_for_all at' f_typ in
    let* x_typ, x_kind = Typ.infer x_typ in
    Kind.unify at' d_kind x_kind
    >> (pure @@ `Inst (at', f, x_typ) <*> Typ.Core.app_of_norm at' f_con x_typ)
  | `Mu (at', f) -> (
    match f with
    | `Lam (at'', i, t, e) ->
      let* t = Typ.check_and_norm t in
      let+ e = Annot.Exp.def i t >> VarEnv.adding i t (check t e) in
      (`Mu (at', `Lam (at'', i, t, e)), t)
    | f ->
      let* f, f_typ = infer f in
      let* d_typ, c_typ = Typ.check_arrow (at f) f_typ in
      Typ.check_sub_of_norm at' c_typ d_typ >> pure (`Mu (at', f), c_typ))
  | `IfElse (at', c, t, e) ->
    let* c = check (`Const (at c, `Bool)) c in
    let* t, t_typ = infer t and* e, e_typ = infer e in
    pure @@ `IfElse (at', c, t, e) <*> Typ.join_of_norm (at e) t_typ e_typ
  | `Product (at', fs) ->
    Row.check fs >> Row.map_er infer fs >>- fun fs ->
    ( `Product (at', List.map (fun (l, (e, _)) -> (l, e)) fs),
      Typ.product at' (List.map (fun (l, (_, t)) -> (l, t)) fs) )
  | `Select (at', p, i) ->
    let* p, p_typ = infer p and* i, i_typ = infer i in
    let* ls = Typ.check_product (at p) p_typ in
    let* ks = Typ.check_atom (at i) i_typ in
    let rec select t_opt ls ks =
      match (ls, ks) with
      | (l, _) :: ls, k :: _ when Label.compare l k < 0 -> select t_opt ls ks
      | (l, _) :: _, k :: _ when 0 < Label.compare l k ->
        fail @@ `Error_product_lacks (at', p_typ, k)
      | (l, t) :: ls, k :: ks ->
        Annot.Label.def k t
        >> Annot.Label.use l (Label.at k)
        >> let* t_opt =
             match t_opt with
             | None -> pure @@ Some t
             | Some t' -> Typ.join_of_norm at' t t' >>- fun t -> Some t
           in
           select t_opt ls ks
      | [], k :: _ -> fail @@ `Error_product_lacks (at', p_typ, k)
      | _, [] -> pure'1 Option.get t_opt
    in
    pure @@ `Select (at', p, i) <*> select None ls ks
  | `Inject (at', l, e) ->
    infer e >>- fun (e, e_typ) -> (`Inject (at', l, e), Typ.sum at' [(l, e_typ)])
  | `Pack (at', t, e, et) ->
    let* t, t_kind = Typ.infer t and* et = Typ.check_and_norm et in
    let* et_con, d_kind = Typ.check_exists at' et in
    Kind.unify at' d_kind t_kind >> Typ.Core.app_of_norm at' et_con t
    >>= fun a ->
    check a e >>- fun e -> (`Pack (at', t, e, et), et)
  | `PackImp (at', _, _) -> fail @@ `Error_exp_lacks_annot at'
  | `UnpackIn (at', tid, k, id, v, e) -> (
    let* v, v_typ = infer v in
    let* v_con, d_kind = Typ.check_exists at' v_typ in
    let* id_typ = Typ.Core.app_of_norm at' v_con @@ `Var (at', tid) in
    let* e, e_typ =
      Kind.unify at' k d_kind >> Annot.Exp.def id id_typ >> infer e
      |> VarEnv.adding id id_typ
      |> Typ.VarEnv.adding tid @@ `Kind d_kind
    in
    Typ.Core.is_free tid e_typ >>= function
    | true -> fail @@ `Error_typ_var_escapes (at', tid, e_typ)
    | false -> pure (`UnpackIn (at', tid, k, id, v, e), e_typ))
  | `Merge (at', l, r) ->
    let select e l = `Select (Loc.dummy, e, atom (Label.set_at Loc.dummy l)) in
    let binding v t e =
      let i = Var.of_string at' "_Merge" |> Var.freshen in
      let+ e = e @@ `Var (at', i) in
      `App (at', `Lam (at', i, t, e), v)
    in
    let rec merge le re lt rt =
      match (lt, rt) with
      | `Row (_, `Product, ls), `Row (_, `Product, rs) ->
        Row.union_er
          (select le >>> pure >>> const)
          (select re >>> pure >>> const)
          (fun l lt rt ->
            binding (select le l) lt @@ fun le ->
            binding (select re l) rt @@ fun re -> merge le re lt rt)
          ls rs
        >>- fun fs -> `Product (at', fs)
      | _ -> fail @@ `Error_non_disjoint_merge (at', lt, rt)
    in
    let* l, l_typ = infer l and* r, r_typ = infer r in
    ( binding l l_typ @@ fun l ->
      binding r r_typ @@ fun r -> merge l r l_typ r_typ )
    >>= fun e -> infer (e : Core.t :> t)

and check a e =
  Typ.Core.unfold_of_norm a >>= function
  | `For (_, `All, _) when cannot_be_for_all e ->
    let at' = at e in
    check a
    @@ `Gen (at', Typ.Var.freshen (Typ.Var.from (a :> Typ.t)), Kind.fresh at', e)
  | _ -> (
    match e with
    | `Lam (at', i, u, e) ->
      let* d, c = Typ.check_arrow at' a and* u = Typ.check_and_norm u in
      Typ.check_sub_of_norm at' d u
      >> Annot.Exp.def i u
      >> VarEnv.adding i u (check c e)
      >>- fun e -> `Lam (at', i, d, e)
    | `LamImp (at', i, e) ->
      let* d, _ = Typ.check_arrow at' a in
      check a @@ `Lam (at', i, (d :> Typ.t), e)
    | `Case (at', cs) ->
      let* d, c = Typ.check_arrow at' a in
      let* ls = Typ.check_sum at' d in
      let cs_typ = Typ.product at' (Row.map (fun d -> `Arrow (at', d, c)) ls) in
      check cs_typ cs >>- fun cs -> `Case (at', cs)
    | `App (at', ((`Lam _ | `LamImp _ | `Case _) as f), x) -> (
      match f with
      | `Lam (at'', d, u, r) ->
        let* u = Typ.check_and_norm u in
        let* x = check u x in
        let+ r = Annot.Exp.def d u >> VarEnv.adding d u (check a r) in
        `App (at', `Lam (at'', d, u, r), x)
      | (`LamImp _ | `Case _) as f ->
        let* x, x_typ = infer x in
        check (`Arrow (at', x_typ, a)) f >>- fun f -> `App (at', f, x))
    | `Gen (at', d, d_kind, r) ->
      let* a_con, d_kind' = Typ.check_for_all at' a in
      let* r_typ = Typ.Core.app_of_norm at' a_con (Typ.var d) in
      Kind.unify at' d_kind d_kind'
      >> Typ.VarEnv.adding d (`Kind d_kind) (check r_typ r)
      >>- fun r -> `Gen (at', d, d_kind', r)
    | `IfElse (at', c, t, e) ->
      let* c = check (`Const (at c, `Bool)) c in
      check a t <*> check a e >>- fun (t, e) -> `IfElse (at', c, t, e)
    | `Mu (at', f) -> check (`Arrow (at', a, a)) f >>- fun f -> `Mu (at', f)
    | `Product (at', fs) ->
      let* las =
        Typ.check_product at' a >>- (List.to_seq >>> LabelMap.of_seq)
      in
      let* leas =
        Row.check fs
        >> List.map_er
             (fun (l, e) ->
               match LabelMap.find_opt l las with
               | None -> infer e >>- fun (e, t) -> (l, e, t)
               | Some a -> check a e >>- fun e -> (l, e, a))
             fs
      in
      Typ.check_sub_of_norm at'
        (Typ.product at' (List.map (fun (l, _, t) -> (l, t)) leas))
        a
      >> pure @@ `Product (at', List.map (fun (l, e, _) -> (l, e)) leas)
    | `Inject (at', l, e) -> (
      Typ.check_sum at' a >>- List.find_opt (fst >>> Label.equal l) >>= function
      | None -> fail @@ `Error_sum_lacks (at', a, l)
      | Some (_, a) -> check a e >>- fun e -> `Inject (at', l, e))
    | `PackImp (at', u, e) -> infer @@ `Pack (at', u, e, (a :> Typ.t)) >>- fst
    | `UnpackIn (at', tid, k, id, v, e) ->
      let* v, v_typ = infer v in
      let* v_con, d_kind = Typ.check_exists at' v_typ in
      let* id_typ = Typ.Core.app_of_norm at' v_con @@ `Var (at', tid) in
      Kind.unify at' k d_kind >> Annot.Exp.def id id_typ >> check a e
      |> VarEnv.adding id id_typ
      |> Typ.VarEnv.adding tid @@ `Kind d_kind
      >>- fun e -> `UnpackIn (at', tid, k, id, v, e)
    | e ->
      let* e, e_typ = infer e in
      Typ.check_sub_of_norm (at e) e_typ a >> pure e)

let infer e =
  let* result = catch @@ infer e in
  Annot.Typ.resolve Kind.resolve
  >> match result with `Ok v -> pure v | `Error e -> fail e
