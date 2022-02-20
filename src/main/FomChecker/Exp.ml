open FomBasis
open FomSource
open FomAnnot

(* *)

include FomAST.Exp

(* *)

module VarMap = struct
  include VarMap

  type t = (Var.t * Typ.Core.t) VarMap.t

  let field r : (t, _) Field.t = r#exp_env
  let adding v t = mapping field @@ VarMap.add v (v, t)
  let find_opt i = get_as field @@ VarMap.find_opt i

  class con =
    object
      val exp_env : t = VarMap.empty
      method exp_env = Field.make exp_env (fun v -> {<exp_env = v>})
    end

  type 'r f = < exp_env : (t, 'r) Field.t >
end

(* *)

module Typ = struct
  include Typ

  let check_and_norm typ = check_and_resolve (`Star (at typ)) typ

  let check_arrow at typ =
    match Core.unfold_of_norm typ with
    | `Arrow (_, dom, cod) -> return (dom, cod)
    | _ -> fail @@ `Error_typ_unexpected (at, "_ → _", typ)

  let check_product at typ =
    match Core.unfold_of_norm typ with
    | `Product (_, ls) -> return ls
    | _ -> fail @@ `Error_typ_unexpected (at, "{_}", typ)

  let check_sum at typ =
    match Core.unfold_of_norm typ with
    | `Sum (_, ls) -> return ls
    | _ -> fail @@ `Error_typ_unexpected (at, "'_", typ)

  let check_unit at typ =
    match Core.unfold_of_norm typ with
    | `Const (_, `Unit) -> unit
    | _ -> fail @@ `Error_typ_unexpected (at, "()", typ)

  let check_atom at typ =
    let* ls = check_sum at typ in
    match ls with
    | [] -> fail @@ `Error_typ_unexpected (at, "'_", typ)
    | ls ->
      ls |> List.iter_fr (snd >>> check_unit at) >> return (List.map fst ls)

  let check_for_all at typ =
    match Core.unfold_of_norm typ with
    | `ForAll (_, f_con) -> (
      let* f_kind = kind_of f_con in
      match f_kind with
      | `Arrow (_, d_kind, `Star _) -> return (f_con, d_kind)
      | _ -> failwith "check_for_all")
    | _ -> fail @@ `Error_typ_unexpected (at, "∀(_)", typ)

  let check_exists at typ =
    match Core.unfold_of_norm typ with
    | `Exists (_, f_con) -> (
      let* f_kind = kind_of f_con in
      match f_kind with
      | `Arrow (_, d_kind, `Star _) -> return (f_con, d_kind)
      | _ -> failwith "check_exists")
    | _ -> fail @@ `Error_typ_unexpected (at, "∃(_)", typ)
end

let rec infer = function
  | `App (at', `Lam (at'', i, t, e), v) ->
    let* t = Typ.check_and_norm t in
    let+ v = check t v
    and+ e, e_typ = Annot.Exp.def i t >> VarMap.adding i t (infer e) in
    (`App (at', `Lam (at'', i, t, e), v), e_typ)
  | `App (at', `LamImp (_, d, r), x) ->
    let* x, x_typ = infer x in
    let+ r, r_typ = Annot.Exp.def d x_typ >> VarMap.adding d x_typ (infer r) in
    (`App (at', `Lam (at', d, x_typ, r), x), r_typ)
  | `Const (at', c) ->
    let+ c = Const.map_typ_fr Typ.check_and_norm c in
    (`Const (at', c), Const.type_of at' c)
  | `Var (at', x) as e -> (
    let* x_typ_opt = VarMap.find_opt x in
    match x_typ_opt with
    | None -> fail @@ `Error_var_unbound (at', x)
    | Some (def, x_typ) -> Annot.Exp.use x (Var.at def) >> return (e, x_typ))
  | `Lam (at', d, d_typ, r) ->
    let* d_typ = Typ.check_and_norm d_typ in
    let+ r, r_typ = Annot.Exp.def d d_typ >> VarMap.adding d d_typ (infer r) in
    (`Lam (at', d, d_typ, r), `Arrow (at', d_typ, r_typ))
  | `App (at', f, x) ->
    let* f, f_typ = infer f in
    let* d_typ, c_typ = Typ.check_arrow (at f) f_typ in
    let+ x = check d_typ x in
    (`App (at', f, x), c_typ)
  | `Gen (at', d, d_kind, r) ->
    let* r, r_typ = infer r |> Typ.VarMap.adding d @@ `Kind d_kind in
    let+ d_kind = Kind.resolve d_kind >>- Kind.ground in
    ( `Gen (at', d, d_kind, r),
      `ForAll (at', Typ.Core.lam_of_norm at' d d_kind r_typ) )
  | `Inst (at', f, x_typ) ->
    let* f, f_typ = infer f in
    let* f_con, d_kind = Typ.check_for_all at' f_typ in
    let* x_typ, x_kind = Typ.infer x_typ in
    Kind.unify at' d_kind x_kind
    >> return (`Inst (at', f, x_typ), Typ.Core.app_of_norm at' f_con x_typ)
  | `Mu (at', `Lam (at'', i, t, e)) ->
    let* t = Typ.check_and_norm t in
    let+ e = Annot.Exp.def i t >> VarMap.adding i t (check t e) in
    (`Mu (at', `Lam (at'', i, t, e)), t)
  | `Mu (at', f) ->
    let* f, f_typ = infer f in
    let* d_typ, c_typ = Typ.check_arrow (at f) f_typ in
    Typ.check_sub_of_norm at' c_typ d_typ >> return (`Mu (at', f), c_typ)
  | `IfElse (at', c, t, e) ->
    let* c = check (`Const (at c, `Bool)) c in
    let* t, t_typ = infer t and* e, e_typ = infer e in
    return @@ `IfElse (at', c, t, e) <*> Typ.join_of_norm (at e) t_typ e_typ
  | `Product (at', fs) ->
    Row.check fs >> Row.map_fr infer fs >>- fun fs ->
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
             | None -> return @@ Some t
             | Some t' -> Typ.join_of_norm at' t t' >>- fun t -> Some t
           in
           select t_opt ls ks
      | [], k :: _ -> fail @@ `Error_product_lacks (at', p_typ, k)
      | _, [] -> return @@ Option.get t_opt
    in
    return @@ `Select (at', p, i) <*> select None ls ks
  | `Inject (at', l, e) ->
    let+ e, e_typ = infer e in
    (`Inject (at', l, e), Typ.sum at' [(l, e_typ)])
  | `Case (at', cs) ->
    let* cs, cs_typ = infer cs in
    let* cs_fs = Typ.check_product (at cs) cs_typ in
    let* cs_arrows = Row.map_fr (Typ.check_arrow (at cs)) cs_fs in
    let+ c_typ =
      match cs_arrows |> List.map (snd >>> snd) with
      | [] -> return @@ Typ.zero (at cs)
      | t :: ts -> ts |> List.fold_left_fr (Typ.join_of_norm (at cs)) t
    in
    let d_typ = `Sum (at cs, cs_arrows |> Row.map fst) in
    (`Case (at', cs), `Arrow (at', d_typ, c_typ))
  | `Pack (at', t, e, et) ->
    let* t, t_kind = Typ.infer t and* et = Typ.check_and_norm et in
    let* et_con, d_kind = Typ.check_exists at' et in
    Kind.unify at' d_kind t_kind >> check (Typ.Core.app_of_norm at' et_con t) e
    >>- fun e -> (`Pack (at', t, e, et), et)
  | `UnpackIn (at', tid, k, id, v, e) ->
    let* v, v_typ = infer v in
    let* v_con, d_kind = Typ.check_exists at' v_typ in
    let id_typ = Typ.Core.app_of_norm at' v_con @@ `Var (at', tid) in
    let* e, e_typ =
      Kind.unify at' k d_kind >> Annot.Exp.def id id_typ >> infer e
      |> VarMap.adding id id_typ
      |> Typ.VarMap.adding tid @@ `Kind d_kind
    in
    if Typ.Core.is_free tid e_typ then
      fail @@ `Error_typ_var_escapes (at', tid, e_typ)
    else return (`UnpackIn (at', tid, k, id, v, e), e_typ)
  | `Merge (at', l, r) ->
    let select e l = `Select (Loc.dummy, e, atom (Label.set_at Loc.dummy l)) in
    let binding v t e =
      let i = Var.of_string at' "_Merge" |> Var.freshen in
      let+ e = e @@ `Var (at', i) in
      `App (at', `Lam (at', i, t, e), v)
    in
    let rec merge le re lt rt =
      match (lt, rt) with
      | `Product (_, ls), `Product (_, rs) ->
        Row.union_fr
          (select le >>> return >>> const)
          (select re >>> return >>> const)
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
  | `LamImp (_, i, _) -> fail @@ `Error_pat_lacks_annot (Var.at i)
  | `PackImp (at', _, _) -> fail @@ `Error_exp_lacks_annot at'

and check a = function
  | `PackImp (at', u, e) ->
    let* a_con, d_kind = Typ.check_exists at' a and* u, u_kind = Typ.infer u in
    Kind.unify at' d_kind u_kind >> check (Typ.Core.app_of_norm at' a_con u) e
    >>- fun e -> `Pack (at', u, e, a)
  | `Lam (at', i, u, e) ->
    let* d, c = Typ.check_arrow at' a and* u = Typ.check_and_norm u in
    Typ.check_sub_of_norm at' d u
    >> Annot.Exp.def i d
    >> VarMap.adding i d (check c e)
    >>- fun e -> `Lam (at', i, d, e)
  | `LamImp (at', i, e) ->
    let* d, c = Typ.check_arrow at' a in
    let+ e = Annot.Exp.def i d >> VarMap.adding i d (check c e) in
    `Lam (at', i, d, e)
  | `App (at', `Lam (at'', d, u, r), x) ->
    let* u = Typ.check_and_norm u in
    let* x = check u x in
    let+ r = Annot.Exp.def d u >> VarMap.adding d u (check a r) in
    `App (at', `Lam (at'', d, u, r), x)
  | `App (at', ((`LamImp _ | `Case _) as f), x) ->
    let* x, x_typ = infer x in
    check (`Arrow (at', x_typ, a)) f >>- fun f -> `App (at', f, x)
  | `Gen (at', d, d_kind, r) ->
    let* a_con, d_kind' = Typ.check_for_all at' a in
    let r_typ = Typ.Core.app_of_norm at' a_con (Typ.var d) in
    Kind.unify at' d_kind d_kind'
    >> Typ.VarMap.adding d (`Kind d_kind) (check r_typ r)
    >>- fun r -> `Gen (at', d, d_kind', r)
  | `IfElse (at', c, t, e) ->
    let* c = check (`Const (at c, `Bool)) c in
    check a t <*> check a e >>- fun (t, e) -> `IfElse (at', c, t, e)
  | `Mu (at', f) -> check (`Arrow (at', a, a)) f >>- fun f -> `Mu (at', f)
  | `Product (at', fs) ->
    let* las = Typ.check_product at' a >>- (List.to_seq >>> LabelMap.of_seq) in
    Row.check fs
    >> List.map_fr
         (fun (l, e) ->
           match LabelMap.find_opt l las with
           | None -> infer e >>- fun (e, _) -> (l, e)
           | Some a -> check a e >>- fun e -> (l, e))
         fs
    >>- fun fs -> `Product (at', fs)
  | `UnpackIn (at', tid, k, id, v, e) ->
    let* v, v_typ = infer v in
    let* v_con, d_kind = Typ.check_exists at' v_typ in
    let id_typ = Typ.Core.app_of_norm at' v_con @@ `Var (at', tid) in
    Kind.unify at' k d_kind >> Annot.Exp.def id id_typ >> check a e
    |> VarMap.adding id id_typ
    |> Typ.VarMap.adding tid @@ `Kind d_kind
    >>- fun e -> `UnpackIn (at', tid, k, id, v, e)
  | `Case (at', cs) ->
    let* d, c = Typ.check_arrow at' a in
    let* ls = Typ.check_sum at' d in
    let cs_typ = `Product (at', Row.map (fun d -> `Arrow (at', d, c)) ls) in
    check cs_typ cs >>- fun cs -> `Case (at', cs)
  | `Inject (at', l, e) -> (
    Typ.check_sum at' a >>- List.find_opt (fst >>> Label.equal l) >>= function
    | None -> fail @@ `Error_sum_lacks (at', a, l)
    | Some (_, a) -> check a e >>- fun e -> `Inject (at', l, e))
  | e ->
    let* e, e_typ = infer e in
    Typ.check_sub_of_norm (at e) e_typ a >> return e

let infer e =
  let* result = catch @@ infer e in
  Annot.Typ.resolve Kind.resolve >> of_res result
