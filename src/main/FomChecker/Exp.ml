open FomBasis
open FomAnnot

(* *)

open Rea

(* *)

include FomAST.Exp

(* *)

module Env = struct
  include Env

  type t = (Id.t * Typ.t) Env.t

  let field r = r#exp_env
  let adding v t = mapping field @@ Env.add v (v, t)
  let find_opt i = get_as field @@ Env.find_opt i

  class con =
    object
      val exp_env : t = Env.empty
      method exp_env = Field.make exp_env (fun v -> {<exp_env = v>})
    end
end

(* *)

module Typ = struct
  include Typ

  let check_and_norm typ = check (`Star (at typ)) typ >> return @@ norm typ

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
    | _ -> fail @@ `Error_typ_unexpected (at, "[_]", typ)

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

let rec infer e = infer_base e >>= Typ.contract

and infer_base = function
  | `Const (at', c) -> Typ.check_and_norm (Const.type_of at' c)
  | `Var (at', x) -> (
    let* x_typ_opt = Env.find_opt x in
    match x_typ_opt with
    | None -> fail @@ `Error_var_unbound (at', x)
    | Some (def, x_typ) -> Annot.Exp.use x (Id.at def) >> return x_typ)
  | `Lam (at', d, d_typ, r) ->
    let* d_typ = Typ.check_and_norm d_typ in
    Annot.Exp.def d d_typ
    >> let+ r_typ = Env.adding d d_typ (infer r) in
       `Arrow (at', d_typ, r_typ)
  | `App (_, f, x) ->
    let* f_typ = infer f in
    let* d_typ, c_typ = Typ.check_arrow (at f) f_typ in
    let* x_typ = infer x in
    Typ.check_sub_of_norm (at x) (x_typ, d_typ) >> return c_typ
  | `Gen (at', d, d_kind, r) ->
    let* r_typ = Typ.Env.adding d d_kind (infer r) in
    Annot.Typ.def d d_kind
    >> return @@ `ForAll (at', Typ.norm (`Lam (at', d, d_kind, r_typ)))
  | `Inst (at', f, x_typ) ->
    let* f_typ = infer f in
    let* f_con, d_kind = Typ.check_for_all at' f_typ in
    let* x_kind = Typ.infer x_typ in
    Kind.check_equal at' d_kind x_kind
    >> return @@ Typ.norm @@ `App (at', f_con, x_typ)
  | `LetIn (_, d, x, r) ->
    let* x_typ = infer x in
    Annot.Exp.def d x_typ >> Env.adding d x_typ (infer r)
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
  | `Select (_, p, l) -> (
    let* p_typ = infer p in
    let* ls = Typ.check_product (at p) p_typ in
    match ls |> List.find_opt (fst >>> Label.equal l) with
    | Some (l', l_typ) ->
      Annot.Label.def l' l_typ
      >> Annot.Label.use l (Label.at l')
      >> return l_typ
    | None -> fail @@ `Error_product_lacks (at p, p_typ, l))
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
    and* t_kind = Typ.infer t
    and* et = Typ.check_and_norm et in
    let* et_con, d_kind = Typ.check_exists at' et in
    Kind.check_equal at' d_kind t_kind
    >>
    let et_t = Typ.norm (`App (at', et_con, t)) in
    Typ.check_sub_of_norm (at e) (e_typ, et_t) >> return et
  | `UnpackIn (at', tid, id, v, e) ->
    let* v_typ = infer v in
    let* v_con, d_kind = Typ.check_exists at' v_typ in
    let id_typ = Typ.norm (`App (at', v_con, `Var (at', tid))) in
    Annot.Exp.def id id_typ >> Annot.Typ.def tid d_kind
    >> let* e_typ =
         infer e |> Env.adding id id_typ |> Typ.Env.adding tid d_kind
       in
       if Typ.is_free tid e_typ then
         fail @@ `Error_typ_var_escapes (at', tid, e_typ)
       else
         return e_typ
  | `Target (_, t, _) -> Typ.check_and_norm t

let infer e =
  let* t = infer e in
  Annot.Typ.resolve return (Typ.norm >>> return) >> return t
