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
  type 'r t = Loc.t -> string -> ('r, e, string) rea

  let dummy at path = fail @@ `Error_file_doesnt_exist (at, path)
  let field r = r#fetch

  class ['r] con (fetch : 'r t) =
    object
      method fetch = fetch
    end

  type 'r f = 'r con

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

  type 'r f = < import_chain : (t, 'r) Field.t >
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

  type nonrec t = t MVar.t

  let empty () = MVar.create empty
  let field r : (t, _) Field.t = r#parameters
  let resetting op = setting field (empty ()) op
  let add filename = mutate field (add filename)
  let get () = read field >>- elements

  let taking_in ast =
    let* imports = env_as ExpImports.field in
    get ()
    >>= List.fold_left_fr
          (fun ast filename ->
            let+ (id, _, typ, _), _ =
              Hashtbl.find imports filename |> LVar.eval
            in
            `Lam (Exp.Var.at id, id, (typ :> Typ.t), ast))
          ast

  let result_without et =
    let rec loop et ps =
      match (et, ps) with
      | (`Lam (_, _, _, e), `Arrow (_, _, t)), _ :: ps -> loop (e, t) ps
      | et, [] -> et
      | _ -> failwith "result_without"
    in
    get () >>- loop et

  class con =
    object
      val parameters : t = empty ()
      method parameters = Field.make parameters (fun v -> {<parameters = v>})
    end

  type 'r f = < parameters : (t, 'r) Field.t >
end

module Elab = struct
  let modularly op =
    op
    |> Typ.VarMap.resetting_to Typ.initial_env
    |> Kind.UnkMap.resetting |> Parameters.resetting
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
  else (i, Typ.VarMap.empty)

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
  | `Annot (_, _, t) -> return t
  | `Const (_, `Unit) as t -> return t
  | `Product (at, fs) -> Row.map_fr type_of_pat_lam fs >>- Typ.product at
  | `Var (at, _) | `Pack (at, _, _, _) -> fail @@ `Error_pat_lacks_annot at

let rec pat_to_exp p' e' = function
  | `Var (at, i) -> `LetIn (at, i, p', e')
  | `Const (at, `Unit) ->
    `App (at, `Lam (at, Exp.Var.fresh at, `Const (at, `Unit), e'), p')
  | `Annot (at, p, t) -> pat_to_exp (`Annot (at, p', t)) e' p
  | `Product (at, []) as t -> `App (at, `Lam (at, Exp.Var.fresh at, t, e'), p')
  | `Product (at, fs) ->
    fs |> List.rev
    |> List.fold_left
         (fun e' (l, p) ->
           let i = Exp.Var.fresh at in
           let e' = pat_to_exp (`Var (at, i)) e' p in
           `LetIn (at, i, `Select (at, p', Exp.atom l), e'))
         e'
  | `Pack (at, `Var (_, i), t, k) -> `UnpackIn (at, t, k, i, p', e')
  | `Pack (at, p, t, k) ->
    let i = Pat.id_for p in
    `UnpackIn (at, t, k, i, p', pat_to_exp (`Var (at, i)) e' p)

let rec elaborate_def = function
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
      |> Typ.VarMap.merging
           (bs |> List.map (fun (i, k, _) -> (i, `Kind k)) |> Typ.VarMap.of_list)
      |> Typ.VarMap.merging (avoiding :> [`Typ of _ | `Kind of _] Typ.VarMap.t)
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
       >>= elaborate_defs Typ.VarMap.empty
     in
     Annot.Typ.resolve Kind.resolve >> return env)
    |> TypIncludes.get_or_put at' inc_path
    |> Elab.modularly

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
    let k = Kind.set_at (Typ.Var.at i) k in
    Annot.Typ.def i k >> elaborate_typ t |> Typ.VarMap.adding i @@ `Kind k
    >>- fun t -> `Lam (at', i, k, t)
  | `App (at', f, x) ->
    elaborate_typ f <*> elaborate_typ x >>- fun (f, x) -> `App (at', f, x)
  | `ForAll (at', t) -> elaborate_typ t >>- fun t -> `ForAll (at', t)
  | `Exists (at', t) -> elaborate_typ t >>- fun t -> `Exists (at', t)
  | `Arrow (at', d, c) ->
    elaborate_typ d <*> elaborate_typ c >>- fun (d, c) -> `Arrow (at', d, c)
  | `Product (at', ls) ->
    Row.map_fr elaborate_typ ls >>- fun ls -> `Product (at', ls)
  | `Sum (at', ls) -> Row.map_fr elaborate_typ ls >>- fun ls -> `Sum (at', ls)
  | `Let (_, def, e) ->
    let* typ_aliases = elaborate_def def in
    elaborate_typ e
    |> Typ.VarMap.merging (typ_aliases :> [`Typ of _ | `Kind of _] Typ.VarMap.t)
  | `Import (at', p) ->
    let sig_path = Path.coalesce at' p |> Path.ensure_ext Path.sig_ext in
    Fetch.fetch at' sig_path
    >>= Parser.parse_utf_8 Grammar.sigs Lexer.offside ~path:sig_path
    >>= elaborate_typ >>= Typ.infer_and_resolve >>- Typ.Core.ground
    |> TypImports.get_or_put at' sig_path
    |> Elab.modularly
    >>- fun t -> (t : Typ.Core.t :> Typ.t)
  | `Join (at', l, r) ->
    elaborate_typ l <*> elaborate_typ r >>- fun (l, r) -> `Join (at', l, r)
  | `Meet (at', l, r) ->
    elaborate_typ l <*> elaborate_typ r >>- fun (l, r) -> `Meet (at', l, r)

and elaborate_defs accum = function
  | #FomCST.Typ.Def.f as d ->
    let+ d = elaborate_def d in
    Typ.VarMap.merge Map.prefer_rhs accum d
  | `In (_, d, ds) ->
    let* d = elaborate_def d in
    elaborate_defs (Typ.VarMap.merge Map.prefer_rhs accum d) ds
    |> Typ.VarMap.merging (d :> [`Typ of _ | `Kind of _] Typ.VarMap.t)
  | `LocalIn (_, d, ds) ->
    let* d = elaborate_def d in
    elaborate_defs accum ds
    |> Typ.VarMap.merging (d :> [`Typ of _ | `Kind of _] Typ.VarMap.t)

let rec elaborate = function
  | `Const (at, c) ->
    Exp.Const.map_typ_fr elaborate_typ c >>- fun c -> `Const (at, c)
  | `Var _ as ast -> return ast
  | `Lam (at, i, t, e) | `LamPat (at, `Annot (_, `Var (_, i), t), e) ->
    elaborate_typ t <*> elaborate e >>- fun (t, e) -> `Lam (at, i, t, e)
  | `App (at, f, x) ->
    elaborate f <*> elaborate x >>- fun (f, x) -> `App (at, f, x)
  | `Gen (at, i, k, e) ->
    avoid i @@ fun i ->
    let k = Kind.set_at (Typ.Var.at i) k in
    Annot.Typ.def i k >> elaborate e |> Typ.VarMap.adding i @@ `Kind k
    >>- fun e -> `Gen (at, i, k, e)
  | `Inst (at, e, t) ->
    elaborate e <*> elaborate_typ t >>- fun (e, t) -> `Inst (at, e, t)
  | `LetIn (at, i, v, e) ->
    elaborate v <*> elaborate e >>- fun (v, e) -> `LetIn (at, i, v, e)
  | `Seq (at, e0, e1) ->
    let+ e0 =
      let at = FomCST.Exp.at e0 in
      elaborate @@ `Annot (at, e0, `Const (at, `Unit))
    and+ e1 = elaborate e1 in
    `LetIn (at, Exp.Var.of_string at "_Seq" |> Exp.Var.freshen, e0, e1)
  | `Let (at, def, e) -> (
    match def with
    | #FomCST.Typ.Def.f as def ->
      let* typ_aliases = elaborate_def def in
      elaborate e
      |> Typ.VarMap.merging
           (typ_aliases :> [`Typ of _ | `Kind of _] Typ.VarMap.t)
    | `PatPar [(p, v)] -> (
      let* v = elaborate v in
      match p with
      | `Var (_, i) -> elaborate e >>- fun e -> `LetIn (at, i, v, e)
      | `Pack (_, `Var (_, ei), ti, k) ->
        avoid ti @@ fun ti ->
        Annot.Typ.def ti k >> elaborate e |> Typ.VarMap.adding ti @@ `Kind k
        >>- fun e -> `UnpackIn (at, ti, k, ei, v, e)
      | p ->
        let i = Pat.id_for p in
        let+ e = elaborate_pat (`Var (at, i)) e p in
        `LetIn (at, i, v, e))
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
  | `UnpackIn (at, ti, k, ei, v, e) ->
    let* v = elaborate v in
    avoid ti @@ fun ti ->
    let k = Kind.set_at (Typ.Var.at ti) k in
    Annot.Typ.def ti k >> elaborate e |> Typ.VarMap.adding ti @@ `Kind k
    >>- fun e -> `UnpackIn (at, ti, k, ei, v, e)
  | `LamPat (at, p, e) ->
    let* t = type_of_pat_lam p >>= elaborate_typ in
    let i = Pat.id_for p in
    let+ e = elaborate_pat (`Var (at, i)) e p in
    `Lam (at, i, t, e)
  | `Annot (at, e, t) ->
    elaborate e <*> elaborate_typ t >>- fun (e, t) ->
    annot at (Exp.Var.of_string at "_Annot" |> Exp.Var.freshen) t e
  | `AppL (at, x, f) ->
    let+ x = elaborate x and+ f = elaborate f in
    let i = Exp.Var.of_string at "_AppL" |> Exp.Var.freshen in
    `LetIn (at, i, x, `App (at, f, `Var (at, i)))
  | `AppR (at, f, x) ->
    elaborate x <*> elaborate f >>- fun (x, f) -> `App (at, f, x)
  | `Merge (at', l, r) ->
    elaborate l <*> elaborate r >>- fun (l, r) -> `Merge (at', l, r)
  | `Tstr (at', semantics, fragments) ->
    let semantics = Exp.var semantics in
    let select l = `Select (Label.at l, semantics, Exp.atom l) in
    let app f x = `App (Exp.at f, f, x) in
    let app2 f x y = app (app f x) y in
    let+ e =
      fragments
      |> List.fold_left_fr
           (fun e -> function
             | `Str s ->
               return @@ app2 (select Label.text') (`Const (at', `String s)) e
             | `Exp (l, v) -> elaborate v >>- fun v -> app2 (select l) v e)
           (select Label.begin')
    in
    app (select Label.finish') e
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
       let* e, t =
         Parameters.taking_in e >>= Exp.infer >>= Parameters.result_without
       in
       let+ parameters = Parameters.get () in
       (i, e, t, parameters))
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
   let* ast, typ =
     Parameters.taking_in ast >>= Exp.infer >>= Parameters.result_without
   in
   let+ parameters = Parameters.get () in
   (ast, typ, parameters))
  |> Elab.modularly
