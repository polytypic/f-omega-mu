open FomBasis
open FomAnnot

(* *)

open Rea

(* *)

include FomAST.Exp

(* *)

module VarMap = struct
  include VarMap

  type t = (Var.t * Typ.t) VarMap.t

  let field r = r#exp_env
  let adding v t = mapping field @@ VarMap.add v (v, t)
  let find_opt i = get_as field @@ VarMap.find_opt i

  class con =
    object
      val exp_env : t = VarMap.empty
      method exp_env = Field.make exp_env (fun v -> {<exp_env = v>})
    end
end

(* *)

module Typ = struct
  include Typ

  let check_and_norm typ =
    let* typ = check (`Star (at typ)) typ in
    resolve typ

  let check_arrow at typ =
    match unfold_of_norm typ with
    | `Arrow (_, dom, cod) -> return (dom, cod)
    | _ -> fail @@ `Error_typ_unexpected (at, "_ → _", typ)

  let check_product at typ =
    match unfold_of_norm typ with
    | `Product (_, ls) -> return ls
    | _ -> fail @@ `Error_typ_unexpected (at, "{_}", typ)

  let check_sum at typ =
    match unfold_of_norm typ with
    | `Sum (_, ls) -> return ls
    | _ -> fail @@ `Error_typ_unexpected (at, "'_", typ)

  let check_unit at typ =
    let* ls = check_product at typ in
    match ls with
    | [] -> unit
    | _ -> fail @@ `Error_typ_unexpected (at, "()", typ)

  let check_atom at typ =
    let* ls = check_sum at typ in
    match ls with
    | [] -> fail @@ `Error_typ_unexpected (at, "'_", typ)
    | ls -> ls |> MList.iter (snd >>> check_unit at) >> return (List.map fst ls)

  let check_for_all at typ =
    match unfold_of_norm typ with
    | `ForAll (_, f_con) -> (
      let* f_kind = kind_of f_con in
      match f_kind with
      | `Arrow (_, d_kind, `Star _) -> return (f_con, d_kind)
      | _ -> failwith "impossible")
    | _ -> fail @@ `Error_typ_unexpected (at, "∀(_)", typ)

  let check_exists at typ =
    match unfold_of_norm typ with
    | `Exists (_, f_con) -> (
      let* f_kind = kind_of f_con in
      match f_kind with
      | `Arrow (_, d_kind, `Star _) -> return (f_con, d_kind)
      | _ -> failwith "impossible")
    | _ -> fail @@ `Error_typ_unexpected (at, "∃(_)", typ)
end

let rec infer = function
  | `Const (at', c) -> Typ.check_and_norm (Const.type_of at' c)
  | `Var (at', x) -> (
    let* x_typ_opt = VarMap.find_opt x in
    match x_typ_opt with
    | None -> fail @@ `Error_var_unbound (at', x)
    | Some (def, x_typ) -> Annot.Exp.use x (Var.at def) >> return x_typ)
  | `Lam (at', d, d_typ, r) ->
    let* d_typ = Typ.check_and_norm d_typ in
    let+ r_typ = Annot.Exp.def d d_typ >> VarMap.adding d d_typ (infer r) in
    `Arrow (at', d_typ, r_typ)
  | `App (_, f, x) ->
    let* f_typ = infer f in
    let* d_typ, c_typ = Typ.check_arrow (at f) f_typ in
    let* x_typ = infer x in
    Typ.check_sub_of_norm (at x) (x_typ, d_typ) >> return c_typ
  | `Gen (at', d, d_kind, r) ->
    let* r_typ =
      Annot.Typ.def d d_kind >> Typ.VarMap.adding d d_kind (infer r)
    in
    let* d_kind = Kind.resolve d_kind >>- Kind.ground in
    return @@ `ForAll (at', Typ.lam_of_norm at' d d_kind r_typ)
  | `Inst (at', f, x_typ) ->
    let* f_typ = infer f in
    let* f_con, d_kind = Typ.check_for_all at' f_typ in
    let* x_typ, x_kind = Typ.infer x_typ in
    Kind.unify at' d_kind x_kind >> return @@ Typ.app_of_norm at' f_con x_typ
  | `LetIn (_, d, x, r) ->
    let* x_typ = infer x in
    Annot.Exp.def d x_typ >> VarMap.adding d x_typ (infer r)
  | `Mu (at', f) ->
    let* f_typ = infer f in
    let* d_typ, c_typ = Typ.check_arrow (at f) f_typ in
    Typ.check_sub_of_norm at' (c_typ, d_typ) >> return c_typ
  | `IfElse (_, c, t, e) ->
    let* c_typ = infer c in
    Typ.check_sub_of_norm (at c) (c_typ, `Const (at c, `Bool))
    >> let* t_typ = infer t and* e_typ = infer e in
       Typ.join_of_norm (at e) (t_typ, e_typ)
  | `Product (at', fs) ->
    let+ fs = fs |> MList.traverse @@ MPair.traverse return infer in
    Typ.product at' fs
  | `Select (at', p, i) ->
    let* p_typ = infer p and* i_typ = infer i in
    let* ls = Typ.check_product (at p) p_typ in
    let* ks = Typ.check_atom (at i) i_typ in
    let rec select t_opt ls ks =
      match (ls, ks) with
      | (l, _) :: ls, k :: _ when Label.compare l k < 0 -> select t_opt ls ks
      | (l, _) :: _, k :: _ when 0 < Label.compare l k ->
        fail @@ `Error_product_lacks (at', p_typ, k)
      | (l, t) :: ls, k :: ks ->
        Annot.Label.def l t
        >> Annot.Label.use k (Label.at l)
        >> let* t_opt =
             match t_opt with
             | None -> return @@ Some t
             | Some t' -> Typ.join_of_norm at' (t, t') >>- fun t -> Some t
           in
           select t_opt ls ks
      | [], k :: _ -> fail @@ `Error_product_lacks (at', p_typ, k)
      | _, [] -> return @@ Option.get t_opt
    in
    select None ls ks
  | `Inject (at', l, e) ->
    let+ e_typ = infer e in
    Typ.sum at' [(l, e_typ)]
  | `Case (at', cs) ->
    let* cs_typ = infer cs in
    let* cs_fs = Typ.check_product (at cs) cs_typ in
    let* cs_arrows =
      cs_fs
      |> MList.traverse @@ MPair.traverse return @@ Typ.check_arrow (at cs)
    in
    let+ c_typ =
      match cs_arrows |> List.map (snd >>> snd) with
      | [] -> return @@ Typ.zero (at cs)
      | t :: ts ->
        ts |> MList.fold_left (fun a t -> Typ.join_of_norm (at cs) (a, t)) t
    in
    let d_typ = `Sum (at cs, cs_arrows |> List.map (Pair.map Fun.id fst)) in
    `Arrow (at', d_typ, c_typ)
  | `Pack (at', t, e, et) ->
    let* e_typ = infer e
    and* t, t_kind = Typ.infer t
    and* et = Typ.check_and_norm et in
    let* et_con, d_kind = Typ.check_exists at' et in
    Kind.unify at' d_kind t_kind
    >>
    let et_t = Typ.app_of_norm at' et_con t in
    Typ.check_sub_of_norm (at e) (e_typ, et_t) >> return et
  | `UnpackIn (at', tid, id, v, e) ->
    let* v_typ = infer v in
    let* v_con, d_kind = Typ.check_exists at' v_typ in
    let id_typ = Typ.app_of_norm at' v_con @@ `Var (at', tid) in
    let* e_typ =
      Annot.Exp.def id id_typ >> Annot.Typ.def tid d_kind
      >> VarMap.adding id id_typ (Typ.VarMap.adding tid d_kind (infer e))
    in
    if Typ.is_free tid e_typ then
      fail @@ `Error_typ_var_escapes (at', tid, e_typ)
    else
      return e_typ

let infer e =
  let* result = catch @@ infer e in
  Annot.Typ.resolve Kind.resolve >> of_res result
