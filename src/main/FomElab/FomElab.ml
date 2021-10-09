open FomBasis
open FomSource
open FomParser
open FomAST
open FomAnnot
open FomChecker
open FomDiag

(* *)

module Path = struct
  let inc_ext = ".fomd"
  let sig_ext = ".fomt"
  let mod_ext = ".fom"

  (* *)

  let is_absolute path = String.is_prefix "/" path

  let ensure_ext ext path =
    if Filename.extension path = ext then path else path ^ ext

  (* *)

  let split_to_origin_and_path uri =
    match String.split_on_char '/' uri with
    | (("https:" | "http:") as proto) :: "" :: host :: path ->
      (Some (String.concat "/" [proto; ""; host]), String.concat "/" path)
    | _ -> (None, uri)

  let join_origin_and_path (origin_opt, path) =
    match origin_opt with
    | None -> path
    | Some origin ->
      if is_absolute path then
        origin ^ path
      else
        origin ^ "/" ^ path

  (* *)

  let is_http path =
    String.is_prefix "https://" path || String.is_prefix "http://" path

  let coalesce loc lit =
    let path = JsonString.to_utf8 lit in
    (if is_http path then
       path |> split_to_origin_and_path
    else
      loc |> Loc.path |> Filename.dirname |> split_to_origin_and_path
      |> Pair.map id @@ fun parent_dir ->
         if is_absolute path then
           path
         else
           parent_dir ^ "/" ^ path)
    |> Pair.map id Filename.canonic
    |> join_origin_and_path
end

module Fetch = struct
  type e = [Error.file_doesnt_exist | Error.io_error]
  type t = Loc.t -> string -> (unit, e, string) rea

  let dummy at path = fail @@ `Error_file_doesnt_exist (at, path)
  let field r = r#fetch

  class con (fetch : t) =
    object
      method fetch = fetch
    end

  let fetch at path =
    invoke (fun r -> field r at path) |> map_error (fun (#e as x) -> x)
end

module PathMap = Map.Make (String)
module PathSet = Set.Make (String)

module ImportChain = struct
  type t = Loc.t PathMap.t

  let field r = r#import_chain

  let with_path at path compute =
    let* include_chain = get field in
    PathMap.find_opt path include_chain
    |> Option.iter_fr (fun previously_at ->
           fail @@ `Error_cyclic_includes (at, path, previously_at))
    >> compute

  class con =
    object
      val import_chain : t = PathMap.empty

      method import_chain =
        Field.make import_chain (fun v -> {<import_chain = v>})
    end
end

module PathTable = struct
  type 'a t = (string, (Error.t, 'a) LVar.t) Hashtbl.t

  let get field key =
    let* hashtbl = env_as field in
    match Hashtbl.find_opt hashtbl key with
    | None -> fail @@ `Error_file_doesnt_exist (Loc.dummy, key)
    | Some var -> LVar.get var |> map_error @@ fun (#Error.t as e) -> e

  let get_or_put field at path compute =
    (let* hashtbl = env_as field in
     match Hashtbl.find_opt hashtbl path with
     | None ->
       let* var = LVar.create compute in
       Hashtbl.replace hashtbl path var;
       LVar.get var
     | Some var -> LVar.get var)
    |> ImportChain.with_path at path
end

module TypIncludes = struct
  type t = ([`Typ of Typ.t] Typ.VarMap.t * Annot.map) PathTable.t

  let create () = Hashtbl.create 100
  let field r = r#typ_includes
  let get_or_put at = PathTable.get_or_put field at

  class con (typ_includes : t) =
    object
      method typ_includes = typ_includes
    end
end

module TypImports = struct
  type t = Typ.t PathTable.t

  let create () = Hashtbl.create 100
  let field r = r#typ_imports
  let get_or_put at = PathTable.get_or_put field at

  class con (typ_imports : t) =
    object
      method typ_imports = typ_imports
    end
end

module ExpImports = struct
  type t = (Exp.Var.t * Exp.t * Typ.t * string list) PathTable.t

  let create () = Hashtbl.create 100
  let field r : t = r#exp_imports
  let get_or_put at = PathTable.get_or_put field at
  let get path = PathTable.get field path

  class con (exp_imports : t) =
    object
      method exp_imports = exp_imports
    end
end

module Parameters = struct
  include PathSet

  type nonrec t = t MVar.t

  let empty () = MVar.create empty
  let field r : (t, _) Field.t = r#parameters
  let resetting op = setting field (empty ()) op

  let add filename =
    let* ps = get field in
    MVar.mutate ps (add filename)

  let get () = get field >>= MVar.get >>- elements

  let taking_in ast =
    let* imports = env_as ExpImports.field in
    get ()
    >>= List.fold_left_fr
          (fun ast filename ->
            let+ id, _, typ, _ = Hashtbl.find imports filename |> LVar.get in
            `Lam (Exp.Var.at id, id, typ, ast))
          ast

  let result_without t =
    let rec loop t ps =
      match (t, ps) with
      | `Arrow (_, _, t), _ :: ps -> loop t ps
      | t, [] -> t
      | _ -> failwith "result_without"
    in
    get () >>- loop t

  class con =
    object
      val parameters : t = empty ()
      method parameters = Field.make parameters (fun v -> {<parameters = v>})
    end
end

module Elab = struct
  let initial_typ_env =
    let open Typ in
    initial_env
    |> VarMap.map (fun k -> `Kind k)
    |> VarMap.add_list
         (let at' = Loc.of_path "prelude" in
          [
            (Var.of_string at' "bool", `Const (at', `Bool));
            (Var.of_string at' "int", `Const (at', `Int));
            (Var.of_string at' "string", `Const (at', `String));
          ]
          |> List.map (fun (i, t) -> (i, `Typ t)))

  let modularly op =
    op
    |> Typ.VarMap.resetting_to initial_typ_env
    |> Kind.UnkMap.resetting |> Parameters.resetting |> Annot.scoping
    |> map_error @@ fun (#Error.t as e) -> e
end

(* *)

let to_avoid_capture i =
  let+ exists =
    Typ.VarMap.existing (fun _ -> function
      | `Typ t' -> Typ.is_free i t' | _ -> false)
  in
  if exists then
    let i' = Typ.Var.freshen i in
    let v' = Typ.var i' in
    (i', Typ.VarMap.singleton i @@ `Typ v')
  else
    (i, Typ.VarMap.empty)

let rec to_avoid_captures = function
  | [] -> return ([], Typ.VarMap.empty)
  | i :: is ->
    let+ i's, es = to_avoid_captures is and+ i', e = to_avoid_capture i in
    (i' :: i's, Typ.VarMap.merge Map.prefer_rhs e es)

let avoid i inn =
  let* i, avoiding = to_avoid_capture i in
  inn i
  |> Typ.VarMap.merging (avoiding :> [`Typ of _ | `Kind of _] Typ.VarMap.t)

(* *)

let annot at i k t = `App (at, `Lam (at, i, k, `Var (at, i)), t)

(* *)

let rec type_of_pat_lam = function
  | `Id (_, _, t) -> t
  | `Product (at, fs) ->
    Typ.product at
      (fs |> Row.map @@ function `Pat p -> type_of_pat_lam p | `Ann t -> t)
  | `Pack (_, _, _, t) -> t

let rec pat_to_exp p' e' = function
  | `Id (at, i, _) -> `LetIn (at, i, p', e')
  | `Product (at, fs) ->
    fs |> List.rev
    |> List.fold_left
         (fun e' -> function
           | l, `Pat p ->
             let i = Exp.Var.freshen (Exp.Var.of_label l) in
             let e' = pat_to_exp (`Var (at, i)) e' p in
             `LetIn (at, i, `Select (at, p', Exp.atom l), e')
           | l, `Ann _ ->
             `LetIn (at, Exp.Var.of_label l, `Select (at, p', Exp.atom l), e'))
         e'
  | `Pack (at, `Id (_, i, _), t, _) -> `UnpackIn (at, t, i, p', e')
  | `Pack (at, p, t, _) ->
    let i = Exp.Var.fresh (FomCST.Exp.Pat.at p) in
    `UnpackIn (at, t, i, p', pat_to_exp (`Var (at, i)) e' p)

let rec elaborate_def = function
  | `Typ (_, i, k, t) ->
    let at = Typ.Var.at i in
    let+ t = elaborate_typ (annot at i k t) >>= Typ.infer_and_resolve in
    Typ.VarMap.singleton i @@ `Typ (Typ.set_at at t)
  | `TypRec (_, bs) ->
    let is = List.map (fun (i, _, _) -> i) bs in
    let* () =
      is |> List.find_dup_opt Typ.Var.compare |> function
      | Some (i, i') -> fail @@ `Error_duplicated_typ_bind (Typ.Var.at i', i)
      | None -> unit
    in
    let* i's, avoiding = to_avoid_captures is in
    let bs = List.map2 (fun i (_, k, t) -> (i, k, t)) i's bs in
    let* () = bs |> List.iter_fr (fun (i, k, _) -> Annot.Typ.def i k) in
    let* assoc =
      bs
      |> List.map_fr (fun (i, k, t) ->
             let at' = Typ.Var.at i in
             let+ t = elaborate_typ t in
             (i, `Mu (at', `Lam (at', i, k, t))))
      |> Typ.VarMap.merging
           (bs |> List.map (fun (i, k, _) -> (i, `Kind k)) |> Typ.VarMap.of_list)
      |> Typ.VarMap.merging (avoiding :> [`Typ of _ | `Kind of _] Typ.VarMap.t)
    in
    assoc
    |> List.map (snd >>> Typ.subst_rec (Typ.VarMap.of_list assoc))
    |> List.map_fr Typ.infer_and_resolve
    >>- (List.map2 (fun i t -> (i, `Typ t)) is >>> Typ.VarMap.of_list)
  | `Include (at', p) ->
    let inc_path = Path.coalesce at' p |> Path.ensure_ext Path.inc_ext in
    let* env, newer =
      (TypIncludes.get_or_put at' inc_path <<< Elab.modularly)
        (let* env =
           Fetch.fetch at' inc_path
           >>= Parser.parse_utf_8 Grammar.typ_defs Lexer.offside ~path:inc_path
           >>= elaborate_defs Typ.VarMap.empty
         in
         Annot.Typ.resolve Kind.resolve >> get Annot.field >>= MVar.get
         >>- fun annot -> (env, annot))
    in
    let* annot = get Annot.field in
    MVar.mutate annot (Annot.LocMap.merge Map.prefer_lhs newer) >> return env

and elaborate_typ = function
  | `Mu (at', t) -> elaborate_typ t >>- fun t -> `Mu (at', t)
  | `Const (_, _) as inn -> return inn
  | `Var (at', i) as inn -> (
    let* t_opt = Typ.VarMap.find_opt i in
    match t_opt with
    | Some (`Typ t) ->
      let t = Typ.freshen t in
      Annot.Typ.use i (Typ.at t) >> return t
    | Some (`Kind k) -> Annot.Typ.use i (Kind.at k) >> return inn
    | _ -> fail @@ `Error_typ_var_unbound (at', i))
  | `Lam (at', i, k, t) ->
    avoid i @@ fun i ->
    elaborate_typ t |> Typ.VarMap.adding i @@ `Kind k >>- fun t ->
    `Lam (at', i, k, t)
  | `App (at', f, x) ->
    let+ f = elaborate_typ f and+ x = elaborate_typ x in
    `App (at', f, x)
  | `ForAll (at', t) -> elaborate_typ t >>- fun t -> `ForAll (at', t)
  | `Exists (at', t) -> elaborate_typ t >>- fun t -> `Exists (at', t)
  | `Arrow (at', d, c) ->
    let+ d = elaborate_typ d and+ c = elaborate_typ c in
    `Arrow (at', d, c)
  | `Product (at', ls) ->
    Row.map_fr elaborate_typ ls >>- fun ls -> `Product (at', ls)
  | `Sum (at', ls) -> Row.map_fr elaborate_typ ls >>- fun ls -> `Sum (at', ls)
  | `LetDefIn (_, def, e) ->
    let* typ_aliases = elaborate_def def in
    elaborate_typ e
    |> Typ.VarMap.merging (typ_aliases :> [`Typ of _ | `Kind of _] Typ.VarMap.t)
  | `Import (at', p) ->
    let sig_path = Path.coalesce at' p |> Path.ensure_ext Path.sig_ext in
    (TypImports.get_or_put at' sig_path <<< Elab.modularly)
      (Fetch.fetch at' sig_path
      >>= Parser.parse_utf_8 Grammar.typ_exp Lexer.offside ~path:sig_path
      >>= elaborate_typ >>= Typ.infer_and_resolve >>- Typ.ground)

and elaborate_defs accum = function
  | [] -> return accum
  | def :: defs ->
    let* typ_aliases =
      elaborate_def def
      |> Typ.VarMap.merging (accum :> [`Typ of _ | `Kind of _] Typ.VarMap.t)
    in
    let accum = Typ.VarMap.merge Map.prefer_lhs typ_aliases accum in
    elaborate_defs accum defs

let maybe_annot e tO =
  match tO with
  | None -> return e
  | Some t ->
    let+ t = elaborate_typ t in
    let at = Typ.at t in
    let i = Exp.Var.fresh at in
    annot at i t e

let rec elaborate = function
  | `Const (at, c) ->
    Exp.Const.map_typ_fr elaborate_typ c >>- fun c -> `Const (at, c)
  | `Var _ as ast -> return ast
  | `Lam (at, i, t, e) | `LamPat (at, `Id (_, i, t), e) ->
    let+ t = elaborate_typ t and+ e = elaborate e in
    `Lam (at, i, t, e)
  | `App (at, f, x) ->
    let+ f = elaborate f and+ x = elaborate x in
    `App (at, f, x)
  | `Gen (at, i, k, e) ->
    avoid i @@ fun i ->
    elaborate e |> Typ.VarMap.adding i @@ `Kind k >>- fun e -> `Gen (at, i, k, e)
  | `Inst (at, e, t) ->
    let+ e = elaborate e and+ t = elaborate_typ t in
    `Inst (at, e, t)
  | `LetIn (at, i, v, e) ->
    let+ v = elaborate v and+ e = elaborate e in
    `LetIn (at, i, v, e)
  | `LetPat (at, `Id (_, i, _), tO, v, e) ->
    let* v = elaborate v in
    let* v = maybe_annot v tO in
    let+ e = elaborate e in
    `LetIn (at, i, v, e)
  | `LetDefIn (_, def, e) ->
    let* typ_aliases = elaborate_def def in
    elaborate e
    |> Typ.VarMap.merging (typ_aliases :> [`Typ of _ | `Kind of _] Typ.VarMap.t)
  | `Mu (at, e) -> elaborate e >>- fun e -> `Mu (at, e)
  | `IfElse (at, c, t, e) ->
    let+ c = elaborate c and* t = elaborate t and+ e = elaborate e in
    `IfElse (at, c, t, e)
  | `Product (at, fs) -> Row.map_fr elaborate fs >>- fun fs -> `Product (at, fs)
  | `Select (at, e, l) ->
    let+ e = elaborate e and+ l = elaborate l in
    `Select (at, e, l)
  | `Inject (at, l, e) -> elaborate e >>- fun e -> `Inject (at, l, e)
  | `Case (at, cs) -> elaborate cs >>- fun cs -> `Case (at, cs)
  | `Pack (at, t, e, x) ->
    let+ t = elaborate_typ t and+ e = elaborate e and+ x = elaborate_typ x in
    `Pack (at, t, e, x)
  | `UnpackIn (at, ti, ei, v, e) ->
    let* v = elaborate v in
    let k = Kind.fresh (Typ.Var.at ti) in
    avoid ti @@ fun ti ->
    Annot.Typ.def ti k >> elaborate e |> Typ.VarMap.adding ti @@ `Kind k
    >>- fun e -> `UnpackIn (at, ti, ei, v, e)
  | `LetPat (at, `Pack (_, `Id (_, ei, _), ti, _), tO, v, e) ->
    let* v = elaborate v in
    let* v = maybe_annot v tO in
    let k = Kind.fresh (Typ.Var.at ti) in
    avoid ti @@ fun ti ->
    Annot.Typ.def ti k >> elaborate e |> Typ.VarMap.adding ti @@ `Kind k
    >>- fun e -> `UnpackIn (at, ti, ei, v, e)
  | `LetPatRec (at, [(p, v)], e) ->
    elaborate @@ `LetPat (at, p, None, `Mu (at, `LamPat (at, p, v)), e)
  | `LetPatRec (at, pvs, e) ->
    let ls = pvs |> List.map (fst >>> FomCST.Exp.Pat.label_for) in
    let p = `Product (at, List.map2 (fun l (p, _) -> (l, `Pat p)) ls pvs) in
    let v = `Product (at, List.map2 (fun l (_, v) -> (l, v)) ls pvs) in
    elaborate @@ `LetPat (at, p, None, `Mu (at, `LamPat (at, p, v)), e)
  | `LamPat (at, p, e) ->
    let t = type_of_pat_lam p in
    let* t = elaborate_typ t in
    let i = Exp.Var.fresh (FomCST.Exp.Pat.at p) in
    let+ e = elaborate_pat (`Var (at, i)) e p in
    `Lam (at, i, t, e)
  | `LetPat (at, p, tO, v, e) ->
    let* v = elaborate v in
    let* v = maybe_annot v tO in
    let i = Exp.Var.fresh (FomCST.Exp.Pat.at p) in
    let+ e = elaborate_pat (`Var (at, i)) e p in
    `LetIn (at, i, v, e)
  | `Annot (at, e, t) ->
    let+ e = elaborate e and+ t = elaborate_typ t in
    let x = Exp.Var.fresh at in
    annot at x t e
  | `AppL (at, x, f) ->
    let+ x = elaborate x and+ f = elaborate f in
    let i = Exp.Var.fresh at in
    `LetIn (at, i, x, `App (at, f, `Var (at, i)))
  | `AppR (at, f, x) ->
    let+ x = elaborate x and+ f = elaborate f in
    `App (at, f, x)
  | `Import (at', p) ->
    let mod_path = Path.coalesce at' p |> Path.ensure_ext Path.mod_ext in
    let sig_path = Filename.remove_extension mod_path ^ Path.sig_ext in
    let* typ_opt =
      (TypImports.get_or_put at' sig_path <<< Elab.modularly)
        (Fetch.fetch at' sig_path
        >>= Parser.parse_utf_8 Grammar.typ_exp Lexer.offside ~path:sig_path
        >>= elaborate_typ >>= Typ.infer_and_resolve >>- Typ.ground)
      |> try_in (fun contents -> return @@ Some contents) @@ function
         | `Error_file_doesnt_exist (_, path) when path = sig_path ->
           return None
         | e -> fail e
    in
    let* id, _, _, _ =
      (ExpImports.get_or_put at' mod_path <<< Elab.modularly)
        (let* ast =
           Fetch.fetch at' mod_path
           >>= Parser.parse_utf_8 Grammar.program Lexer.offside ~path:mod_path
           >>= elaborate >>- Exp.initial_exp
         in
         let id = Exp.Var.fresh at' in
         let ast =
           match typ_opt with None -> ast | Some typ -> annot at' id typ ast
         in
         let* typ =
           Parameters.taking_in ast >>= Exp.infer >>= Parameters.result_without
         in
         let+ parameters = Parameters.get () in
         (id, ast, typ, parameters))
    in
    Parameters.add mod_path >> return @@ `Var (at', id)

and elaborate_pat p' e' p =
  FomCST.Exp.Pat.check p >>= fun () -> pat_to_exp p' e' p |> elaborate

(* *)

let elaborate_typ x = Elab.modularly (elaborate_typ x)

(* *)

let elaborate cst =
  Elab.modularly
    (let* ast = elaborate cst >>- Exp.initial_exp in
     let* typ =
       Parameters.taking_in ast >>= Exp.infer >>= Parameters.result_without
     in
     let+ parameters = Parameters.get () in
     (ast, typ, parameters))
