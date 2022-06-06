open FomBasis
open FomSource
open FomParser
open FomAST
open FomAnnot
open FomChecker
open FomError

(* *)

module Pat = struct
  include FomCST.Exp.Pat

  let id_for p =
    "_" ^ to_string p
    |> Lexer.coerce_to_id
    |> Exp.Var.of_string (at p)
    |> Exp.Var.freshen

  let label_for p = to_string p |> Label.of_string Loc.dummy
end

module Annot = struct
  include Annot

  let setup op =
    let open FomAST in
    Typ.initial_env |> Typ.VarMap.bindings
    |> List.iter_fr (function
         | i, `Kind k -> Annot.Typ.def i k
         | i, `Typ _ -> Annot.Typ.def i (`Star (Typ.Var.at i)))
    >> op
end

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
    | Some origin -> origin ^ if is_absolute path then path else "/" ^ path

  (* *)

  let is_http path =
    String.is_prefix "https://" path || String.is_prefix "http://" path

  let coalesce loc lit =
    let path = JsonString.to_utf8 lit in
    (if is_http path then path |> split_to_origin_and_path
    else
      loc |> Loc.path |> Filename.dirname |> split_to_origin_and_path
      |> Pair.map id @@ fun parent_dir ->
         if is_absolute path then path else parent_dir ^ "/" ^ path)
    |> Pair.map id Filename.canonic
    |> join_origin_and_path
end

module Fetch = struct
  type e = [Error.file_doesnt_exist | Error.io_error]
  type 'r m = Loc.t -> string -> ('r, e, string) rea
  type 'r f = < fetch : 'r m >

  let dummy at path = fail @@ `Error_file_doesnt_exist (at, path)
  let field r : _ m = r#fetch

  class con fetch =
    object (_ : 'r)
      method fetch : 'r m = fetch
    end

  let fetch at path =
    invoke (fun r -> field r at path) |> map_error (fun (#e as x) -> x)
end

module PathMap = Map.Make (String)
module PathSet = Set.Make (String)

module ImportChain = struct
  type 'r m = (Loc.t PathMap.t, 'r) Field.t
  type 'r f = < import_chain : 'r m >

  let field r : _ m = r#import_chain

  let with_path at path compute =
    let* include_chain = get field in
    PathMap.find_opt path include_chain
    |> Option.iter_fr (fun previously_at ->
           fail @@ `Error_cyclic_includes (at, path, previously_at))
    >> compute

  class con =
    object
      val import_chain = PathMap.empty

      method import_chain : _ m =
        Field.make import_chain (fun v -> {<import_chain = v>})
    end
end

module PathTable = struct
  type 'a t = (string, (Error.t, 'a * Annot.map) LVar.t) Hashtbl.t

  let get_or_put field at path compute =
    (let* result, inner =
       Annot.scoping
         (let* (hashtbl : _ t) = env_as field in
          match Hashtbl.find_opt hashtbl path with
          | None ->
            let* var = LVar.create (Annot.setup compute <*> read Annot.field) in
            Hashtbl.replace hashtbl path var;
            LVar.eval var
          | Some var -> LVar.eval var)
     in
     mutate Annot.field (Annot.merge inner) >> return result)
    |> ImportChain.with_path at path

  let get field key =
    let* hashtbl = env_as field in
    match Hashtbl.find_opt hashtbl key with
    | None -> fail @@ `Error_file_doesnt_exist (Loc.dummy, key)
    | Some var -> LVar.eval var |> map_error @@ fun (#Error.t as e) -> e
end

module TypIncludes = struct
  type t = [`Typ of Typ.t] Typ.VarMap.t PathTable.t

  let create () = Hashtbl.create 100
  let field r : t = r#typ_includes
  let get_or_put at = PathTable.get_or_put field at

  class con (typ_includes : t) =
    object
      method typ_includes = typ_includes
    end

  type 'r f = con
end

module TypImports = struct
  type t = Typ.Core.t PathTable.t

  let create () = Hashtbl.create 100
  let field r : t = r#typ_imports
  let get_or_put at = PathTable.get_or_put field at

  class con (typ_imports : t) =
    object
      method typ_imports = typ_imports
    end

  type 'r f = con
end

module ExpImports = struct
  type t = (Exp.Var.t * Exp.Core.t * Typ.Core.t * string list) PathTable.t

  let create () = Hashtbl.create 100
  let field r : t = r#exp_imports
  let get_or_put at = PathTable.get_or_put field at
  let get path = PathTable.get field path

  class con (exp_imports : t) =
    object
      method exp_imports = exp_imports
    end

  type 'r f = con
end

module Parameters = struct
  include PathSet

  type 'r m = (t MVar.t, 'r) Field.t
  type 'r f = < parameters : 'r m >

  let empty () = MVar.create empty
  let field r : _ m = r#parameters
  let resetting op = setting field (empty ()) op
  let add filename = mutate field (add filename)
  let get eta = (read field >>- elements) eta

  let taking_in ast =
    let* imports = env_as ExpImports.field in
    get
    >>= List.fold_left_fr
          (fun ast filename ->
            let+ (i, _, t, _), _ = Hashtbl.find imports filename |> LVar.eval in
            `Lam (Exp.Var.at i, i, (t :> Typ.t), ast))
          ast

  let without et =
    let rec loop et ps =
      match (et, ps) with
      | (`Lam (_, _, _, e), `Arrow (_, _, t)), _ :: ps -> loop (e, t) ps
      | et, [] -> et
      | _ -> failwith "without"
    in
    get >>- loop et

  class con =
    object
      val parameters = empty ()

      method parameters : _ m =
        Field.make parameters (fun v -> {<parameters = v>})
    end
end

module Elab = struct
  let modularly op =
    op
    |> Typ.VarEnv.resetting_to Typ.initial_env
    |> Kind.UnkEnv.resetting |> Parameters.resetting
    |> map_error @@ fun (#Error.t as e) -> e
end

(* *)

let to_avoid_capture i =
  let+ exists =
    Typ.VarEnv.existing_fr (fun _ -> function
      | `Typ t' -> Typ.is_free i t' | _ -> return false)
  in
  if exists then
    let i' = Typ.Var.freshen i in
    let v' = Typ.var i' in
    (i', Typ.VarMap.singleton i @@ `Typ v')
  else (i, Typ.VarMap.empty)

let rec to_avoid_captures = function
  | [] -> return ([], Typ.VarMap.empty)
  | i :: is ->
    let+ i's, es = to_avoid_captures is and+ i', e = to_avoid_capture i in
    (i' :: i's, Typ.VarMap.merge Map.prefer_rhs e es)

let avoid i inn =
  let* i, avoiding = to_avoid_capture i in
  inn i
  |> Typ.VarEnv.merging (avoiding :> [`Typ of _ | `Kind of _] Typ.VarMap.t)

(* *)

let annot at i k t = `App (at, `Lam (at, i, k, `Var (at, i)), t)

let lam at i t_opt e =
  match t_opt with Some t -> `Lam (at, i, t, e) | None -> `LamImp (at, i, e)

(* *)

let rec type_of_pat_lam = function
  | `Annot (_, _, t) -> return t
  | `Const (_, `Unit) as t -> return t
  | `Product (at, fs) -> Row.map_fr type_of_pat_lam fs >>- Typ.product at
  | `Var _ | `Pack _ -> zero

let rec pat_to_exp p' e' = function
  | `Var (at, i) -> `App (at, `LamImp (at, i, e'), p')
  | `Const (at, `Unit) as t -> `App (at, `Lam (at, Exp.Var.fresh at, t, e'), p')
  | `Product (at, []) ->
    `App (at, `Lam (at, Exp.Var.fresh at, Typ.product at [], e'), p')
  | `Annot (at, p, t) -> pat_to_exp (`Annot (at, p', t)) e' p
  | `Product (at, fs) ->
    fs |> List.rev
    |> List.fold_left
         (fun e' (l, p) ->
           let i = Exp.Var.fresh at in
           let e' = pat_to_exp (`Var (at, i)) e' p in
           `App (at, `LamImp (at, i, e'), `Select (at, p', Exp.atom l)))
         e'
  | `Pack (at, `Var (_, i), t, k) -> `UnpackIn (at, t, k, i, p', e')
  | `Pack (at, p, t, k) ->
    let i = Pat.id_for p in
    `UnpackIn (at, t, k, i, p', pat_to_exp (`Var (at, i)) e' p)

let rec elaborate_typ_def = function
  | `TypPar ikts ->
    ikts
    |> List.map_fr (fun (i, k, t) ->
           let at = Typ.Var.at i in
           let+ t = elaborate_typ (annot at i k t) >>= Typ.infer_and_resolve in
           (i, `Typ (Typ.Core.set_at at t :> Typ.t)))
    >>- Typ.VarMap.of_list
  | `TypRec bs ->
    let is = List.map (fun (i, _, _) -> i) bs in
    let* () =
      is |> List.find_dup_opt Typ.Var.compare |> function
      | Some (i, i') -> fail @@ `Error_duplicated_typ_bind (Typ.Var.at i', i)
      | None -> unit
    in
    let* i's, avoiding = to_avoid_captures is in
    let bs =
      List.map2 (fun i (_, k, t) -> (i, Kind.set_at (Typ.Var.at i) k, t)) i's bs
    in
    let* () = bs |> List.iter_fr (fun (i, k, _) -> Annot.Typ.def i k) in
    let* assoc =
      bs
      |> List.map_fr (fun (i, k, t) ->
             let at' = Typ.Var.at i in
             let+ t = elaborate_typ t in
             (i, `Mu (at', `Lam (at', i, k, t))))
      |> Typ.VarEnv.merging
           (bs |> List.map (fun (i, k, _) -> (i, `Kind k)) |> Typ.VarMap.of_list)
      |> Typ.VarEnv.merging (avoiding :> [`Typ of _ | `Kind of _] Typ.VarMap.t)
    in
    assoc
    |> List.map (snd >>> Typ.subst_rec (Typ.VarMap.of_list assoc))
    |> List.map_fr Typ.infer_and_resolve
    >>- (List.map2
           (fun i (t : Typ.Core.t) ->
             (i, `Typ (Typ.set_at (Typ.Var.at i) (t :> Typ.t))))
           is
        >>> Typ.VarMap.of_list)
  | `Include (at', p) ->
    let inc_path = Path.coalesce at' p |> Path.ensure_ext Path.inc_ext in
    (let* env =
       Fetch.fetch at' inc_path
       >>= Parser.parse_utf_8 Grammar.incs Lexer.offside ~path:inc_path
       >>= elaborate_typ_defs Typ.VarMap.empty
     in
     Annot.Typ.resolve Kind.resolve >> return env)
    |> TypIncludes.get_or_put at' inc_path
    |> Elab.modularly

and elaborate_typ_defs accum = function
  | #FomCST.Typ.Def.f as d ->
    elaborate_typ_def d >>- Typ.VarMap.merge Map.prefer_rhs accum
  | `In (_, d, ds) ->
    let* d = elaborate_typ_def d in
    elaborate_typ_defs (Typ.VarMap.merge Map.prefer_rhs accum d) ds
    |> Typ.VarEnv.merging (d :> [`Typ of _ | `Kind of _] Typ.VarMap.t)
  | `LocalIn (_, d, ds) ->
    let* d = elaborate_typ_def d in
    elaborate_typ_defs accum ds
    |> Typ.VarEnv.merging (d :> [`Typ of _ | `Kind of _] Typ.VarMap.t)

and elaborate_typ = function
  | `Var (_, i) as inn -> (
    Typ.VarEnv.find i >>= function
    | `Typ t ->
      let t = Typ.freshen t in
      Annot.Typ.use i (Typ.at t) >> return t
    | `Kind k -> Annot.Typ.use i (Kind.at k) >> return inn)
  | `Lam (at', i, k, t) ->
    avoid i @@ fun i ->
    let k = Kind.set_at (Typ.Var.at i) k in
    Annot.Typ.def i k >> elaborate_typ t |> Typ.VarEnv.adding i @@ `Kind k
    >>- fun t -> `Lam (at', i, k, t)
  | `Let (_, def, e) ->
    let* typ_aliases = elaborate_typ_def def in
    elaborate_typ e
    |> Typ.VarEnv.merging (typ_aliases :> [`Typ of _ | `Kind of _] Typ.VarMap.t)
  | `Annot (at', t, k) ->
    elaborate_typ t
    >>- annot at' (Typ.Var.of_string at' "_Annot" |> Typ.Var.freshen) k
  | `Import (at', p) ->
    let sig_path = Path.coalesce at' p |> Path.ensure_ext Path.sig_ext in
    Fetch.fetch at' sig_path
    >>= Parser.parse_utf_8 Grammar.sigs Lexer.offside ~path:sig_path
    >>= elaborate_typ >>= Typ.infer_and_resolve >>- Typ.Core.ground
    |> TypImports.get_or_put at' sig_path
    |> Elab.modularly
    >>- fun t -> (t : Typ.Core.t :> Typ.t)
  | `Unk _ -> failwith "elaborate_typ `Unk"
  | #Typ.f as t -> Typ.map_fr elaborate_typ t

let rec elaborate = function
  | `Const (at, c) ->
    Exp.Const.map_typ_fr elaborate_typ c >>- fun c -> `Const (at, c)
  | `Var _ as ast -> return ast
  | `LamImp (at, i, e) | `LamPat (at, `Var (_, i), e) ->
    elaborate e >>- fun e -> `LamImp (at, i, e)
  | `Lam (at, i, t, e) | `LamPat (at, `Annot (_, `Var (_, i), t), e) ->
    elaborate_typ t <*> elaborate e >>- fun (t, e) -> `Lam (at, i, t, e)
  | `App (at, f, x) ->
    elaborate f <*> elaborate x >>- fun (f, x) -> `App (at, f, x)
  | `Gen (at, i, k, e) ->
    avoid i @@ fun i ->
    let k = Kind.set_at (Typ.Var.at i) k in
    Annot.Typ.def i k >> elaborate e |> Typ.VarEnv.adding i @@ `Kind k
    >>- fun e -> `Gen (at, i, k, e)
  | `Inst (at, e, t) ->
    elaborate e <*> elaborate_typ t >>- fun (e, t) -> `Inst (at, e, t)
  | `Seq (at, e0, e1) ->
    let t = `Const (at, `Unit)
    and i = Exp.Var.of_string at "_Seq" |> Exp.Var.freshen in
    let+ e0 = elaborate e0 and+ e1 = elaborate e1 in
    `App (at, `Lam (at, i, t, e1), e0)
  | `Let (at, def, e) -> (
    match def with
    | #FomCST.Typ.Def.f as def ->
      let* typ_aliases = elaborate_typ_def def in
      elaborate e
      |> Typ.VarEnv.merging
           (typ_aliases :> [`Typ of _ | `Kind of _] Typ.VarMap.t)
    | `PatPar [(p, v)] -> (
      let* v = elaborate v in
      match p with
      | `Var (_, i) -> elaborate e >>- fun e -> `App (at, `LamImp (at, i, e), v)
      | `Pack (_, `Var (_, ei), ti, k) ->
        avoid ti @@ fun ti ->
        Annot.Typ.def ti k >> elaborate e |> Typ.VarEnv.adding ti @@ `Kind k
        >>- fun e -> `UnpackIn (at, ti, k, ei, v, e)
      | p ->
        let* t_opt =
          type_of_pat_lam p |> Option.run |> Option.map_fr elaborate_typ
        in
        let i = Pat.id_for p in
        let+ e = elaborate_pat (`Var (at, i)) e p in
        `App (at, lam at i t_opt e, v))
    | `PatPar pvs ->
      let ls = pvs |> List.map (fst >>> Pat.label_for) in
      let p = `Product (at, List.map2 (fun l (p, _) -> (l, p)) ls pvs) in
      let v = `Product (at, List.map2 (fun l (_, v) -> (l, v)) ls pvs) in
      elaborate @@ `Let (at, `PatPar [(p, v)], e)
    | `PatRec [(p, v)] ->
      elaborate @@ `Let (at, `PatPar [(p, `Mu (at, `LamPat (at, p, v)))], e)
    | `PatRec pvs ->
      let ls = pvs |> List.map (fst >>> Pat.label_for) in
      let p = `Product (at, List.map2 (fun l (p, _) -> (l, p)) ls pvs) in
      let v = `Product (at, List.map2 (fun l (_, v) -> (l, v)) ls pvs) in
      elaborate @@ `Let (at, `PatPar [(p, `Mu (at, `LamPat (at, p, v)))], e))
  | `Mu (at, e) -> elaborate e >>- fun e -> `Mu (at, e)
  | `IfElse (at, c, t, e) ->
    let+ c = elaborate c and* t = elaborate t and+ e = elaborate e in
    `IfElse (at, c, t, e)
  | `Product (at, fs) -> Row.map_fr elaborate fs >>- fun fs -> `Product (at, fs)
  | `Select (at, e, l) ->
    elaborate e <*> elaborate l >>- fun (e, l) -> `Select (at, e, l)
  | `Inject (at, l, e) -> elaborate e >>- fun e -> `Inject (at, l, e)
  | `Case (at, cs) -> elaborate cs >>- fun cs -> `Case (at, cs)
  | `Pack (at, t, e, x) ->
    let+ t = elaborate_typ t and+ e = elaborate e and+ x = elaborate_typ x in
    `Pack (at, t, e, x)
  | `PackImp (at, t, e) ->
    elaborate_typ t <*> elaborate e >>- fun (t, e) -> `PackImp (at, t, e)
  | `UnpackIn (at, ti, k, ei, v, e) ->
    let* v = elaborate v in
    avoid ti @@ fun ti ->
    let k = Kind.set_at (Typ.Var.at ti) k in
    Annot.Typ.def ti k >> elaborate e |> Typ.VarEnv.adding ti @@ `Kind k
    >>- fun e -> `UnpackIn (at, ti, k, ei, v, e)
  | `LamPat (at, p, e) ->
    let* t_opt =
      type_of_pat_lam p |> Option.run |> Option.map_fr elaborate_typ
    in
    let i = Pat.id_for p in
    let+ e = elaborate_pat (`Var (at, i)) e p in
    lam at i t_opt e
  | `Annot (at, e, t) ->
    elaborate e <*> elaborate_typ t >>- fun (e, t) ->
    annot at (Exp.Var.of_string at "_Annot" |> Exp.Var.freshen) t e
  | `AppL (at, x, f) ->
    let+ x = elaborate x and+ f = elaborate f in
    let i = Exp.Var.of_string at "_AppL" |> Exp.Var.freshen in
    `App (at, `LamImp (at, i, `App (at, f, `Var (at, i))), x)
  | `AppR (at, f, x) ->
    elaborate x <*> elaborate f >>- fun (x, f) -> `App (at, f, x)
  | `Merge (at', l, r) ->
    elaborate l <*> elaborate r >>- fun (l, r) -> `Merge (at', l, r)
  | `Tstr (at', semantics, fragments) ->
    let semantics = Exp.var semantics in
    let select l = `Select (Label.at l, semantics, Exp.atom l) in
    let app f x = `App (Exp.at f, f, x) in
    let app2 f x y = app (app f x) y in
    fragments
    |> List.fold_left_fr
         (fun e -> function
           | `Str s ->
             return @@ app2 (select Label.text') (`Const (at', `String s)) e
           | `Exp (l, v) -> elaborate v >>- fun v -> app2 (select l) v e)
         (select Label.begin')
    >>- app (select Label.finish')
  | `Import (at', p) ->
    let mod_path = Path.coalesce at' p |> Path.ensure_ext Path.mod_ext in
    let sig_path = Filename.remove_extension mod_path ^ Path.sig_ext in
    let* t_opt =
      Fetch.fetch at' sig_path
      >>= Parser.parse_utf_8 Grammar.sigs Lexer.offside ~path:sig_path
      >>= elaborate_typ >>= Typ.infer_and_resolve >>- Typ.Core.ground
      |> TypImports.get_or_put at' sig_path
      |> Elab.modularly
      |> try_in (fun contents -> return @@ Some contents) @@ function
         | `Error_file_doesnt_exist (_, path) when path = sig_path ->
           return None
         | e -> fail e
    in
    let* id, _, _, _ =
      (let* e =
         Fetch.fetch at' mod_path
         >>= Parser.parse_utf_8 Grammar.mods Lexer.offside ~path:mod_path
         >>= elaborate >>- Exp.initial_exp
       in
       let i =
         "_" ^ Lexer.coerce_to_id mod_path
         |> Exp.Var.of_string at' |> Exp.Var.freshen
       in
       let e =
         match t_opt with None -> e | Some t -> annot at' i (t :> Typ.t) e
       in
       let+ e, t = Parameters.taking_in e >>= Exp.infer >>= Parameters.without
       and+ ps = Parameters.get in
       (i, e, t, ps))
      |> ExpImports.get_or_put at' mod_path
      |> Elab.modularly
    in
    Parameters.add mod_path >> return @@ `Var (at', id)

and elaborate_pat p' e' p =
  FomCST.Exp.Pat.check p >>= fun () -> pat_to_exp p' e' p |> elaborate

(* *)

let elaborate_typ x = elaborate_typ x |> Annot.setup |> Elab.modularly

(* *)

let elaborate cst =
  (let* ast = elaborate cst |> Annot.setup >>- Exp.initial_exp in
   let+ ast, typ = Parameters.taking_in ast >>= Exp.infer >>= Parameters.without
   and+ ps = Parameters.get in
   (ast, typ, ps))
  |> Elab.modularly
