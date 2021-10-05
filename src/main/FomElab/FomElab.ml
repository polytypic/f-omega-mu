open FomBasis
open FomSource
open FomParser
open FomAST
open FomAnnot
open FomChecker
open FomDiag

(* *)

open Rea

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
      |> Pair.map Fun.id @@ fun parent_dir ->
         if is_absolute path then
           path
         else
           parent_dir ^ "/" ^ path)
    |> Pair.map Fun.id Filename.canonic
    |> join_origin_and_path
end

module Fetch = struct
  type e = [Error.file_doesnt_exist | Error.io_error]
  type t = Loc.t -> string -> (unit, e, string) Rea.t

  let dummy at path = Rea.fail @@ `Error_file_doesnt_exist (at, path)
  let field r = r#fetch

  class con (fetch : t) =
    object
      method fetch = fetch
    end

  let fetch at path =
    invoke (fun r -> field r at path) |> map_error (fun (#e as x) -> x)
end

module TypAliases = struct
  include FomAST.Typ.VarMap

  let empty =
    let at = Loc.of_path "prelude" in
    [
      (Typ.Var.of_string Loc.dummy "bool", `Const (at, `Bool));
      (Typ.Var.of_string Loc.dummy "int", `Const (at, `Int));
      (Typ.Var.of_string Loc.dummy "string", `Const (at, `String));
    ]
    |> List.to_seq |> of_seq

  type nonrec t = FomAST.Typ.t t

  let field r = r#typ_aliases
  let setting e = setting field e
  let find_opt i = get_as field @@ find_opt i

  class con =
    object
      val typ_aliases : t = empty
      method typ_aliases = Field.make typ_aliases (fun v -> {<typ_aliases = v>})
    end
end

module VarTbl = struct
  let get field key =
    let* hashtbl = env_as field in
    match Hashtbl.find_opt hashtbl key with
    | None -> fail @@ `Error_file_doesnt_exist (Loc.dummy, key)
    | Some var -> IVar.get var

  let get_or_put field key compute =
    let* hashtbl = env_as field in
    match Hashtbl.find_opt hashtbl key with
    | None ->
      let var = IVar.empty () in
      Hashtbl.replace hashtbl key var;
      catch compute >>= IVar.put var >> IVar.get var
    | Some var -> IVar.get var
end

module Error = struct
  type t =
    [ Error.io_error
    | Error.syntax_errors
    | Error.source_errors
    | Error.kind_errors
    | Error.type_errors ]

  let generalize x = map_error (fun (#t as x) -> x) x
end

module TypIncludes = struct
  type t =
    ( string,
      (Error.t, FomAST.Typ.t FomAST.Typ.VarMap.t * Annot.map) IVar.t )
    Hashtbl.t

  let create () = Hashtbl.create 100
  let field r = r#typ_includes

  let get_or_put path compute =
    VarTbl.get_or_put field path compute |> Error.generalize

  class con (typ_includes : t) =
    object
      method typ_includes = typ_includes
    end
end

module TypImports = struct
  type t = (string, (Error.t, FomAST.Typ.t) IVar.t) Hashtbl.t

  let create () = Hashtbl.create 100
  let field r = r#typ_imports

  let get_or_put path compute =
    VarTbl.get_or_put field path compute |> Error.generalize

  class con (typ_imports : t) =
    object
      method typ_imports = typ_imports
    end
end

module ExpImports = struct
  type t =
    ( string,
      ( Error.t,
        FomAST.Exp.Var.t * FomAST.Exp.t * FomAST.Typ.t * string list )
      IVar.t )
    Hashtbl.t

  let create () = Hashtbl.create 100
  let field r : t = r#exp_imports

  let get_or_put path compute =
    VarTbl.get_or_put field path compute |> Error.generalize

  let get path = VarTbl.get field path |> Error.generalize

  class con (exp_imports : t) =
    object
      method exp_imports = exp_imports
    end
end

module PathMap = Map.Make (String)
module PathSet = Set.Make (String)

module ImportChain = struct
  type t = Loc.t PathMap.t

  let field r = r#import_chain

  let with_path at path compute =
    let* include_chain = get field in
    PathMap.find_opt path include_chain
    |> MOption.iter (fun previously_at ->
           fail @@ `Error_cyclic_includes (at, path, previously_at))
    >> compute

  class con =
    object
      val import_chain : t = PathMap.empty

      method import_chain =
        Field.make import_chain (fun v -> {<import_chain = v>})
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
    >>= MList.fold_left
          (fun ast filename ->
            let+ id, _, typ, _ = Hashtbl.find imports filename |> IVar.get in
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
  let modularly op =
    op
    |> TypAliases.setting TypAliases.empty
    |> Typ.VarMap.resetting |> Kind.UnkMap.resetting |> Parameters.resetting
    |> Annot.scoping
end

(* *)

let avoid i inn =
  mapping TypAliases.field (TypAliases.remove i)
  @@ let* exists =
       get_as TypAliases.field
         (TypAliases.exists (fun _ t' -> Typ.is_free i t'))
     in
     if exists then
       let i' = Typ.Var.freshen i in
       let v' = Typ.var i' in
       mapping TypAliases.field (TypAliases.add i v') (inn i')
     else
       inn i

let rec type_of_pat_lam = function
  | `Id (_, _, t) -> t
  | `Product (at, fs) ->
    Typ.product at
      (fs |> Row.map @@ function `Pat p -> type_of_pat_lam p | `Ann t -> t)
  | `Pack (_, _, _, t) -> t

let rec elaborate_pat p' e' = function
  | `Id (at, i, _) -> `LetIn (at, i, p', e')
  | `Product (at, fs) ->
    fs |> List.rev
    |> List.fold_left
         (fun e' -> function
           | l, `Pat p ->
             let i =
               Exp.Var.freshen (Exp.Var.of_name (Label.at l) (Label.name l))
             in
             `LetIn
               ( at,
                 i,
                 `Select (at, p', FomCST.Exp.atom l),
                 elaborate_pat (`Var (at, i)) e' p )
           | l, `Ann _ ->
             `LetIn
               ( at,
                 Exp.Var.of_name (Label.at l) (Label.name l),
                 `Select (at, p', FomCST.Exp.atom l),
                 e' ))
         e'
  | `Pack (at, `Id (_, i, _), t, _) -> `UnpackIn (at, t, i, p', e')
  | `Pack (at, p, t, _) ->
    let i = Exp.Var.fresh (FomCST.Exp.Pat.at p) in
    `UnpackIn (at, t, i, p', elaborate_pat (`Var (at, i)) e' p)

let rec elaborate_def = function
  | `Typ (_, i, k, t) ->
    let at = Typ.Var.at i in
    let* t =
      `App (at, `Lam (at, i, k, `Var (at, i)), t)
      |> elaborate_typ >>= Typ.infer_and_resolve
    in
    get_as TypAliases.field (TypAliases.add i t)
  | `TypRec (_, bs) ->
    let* assoc =
      bs
      |> MList.traverse @@ fun (i, k, t) ->
         let at = Typ.Var.at i in
         let t = `Mu (at, `Lam (at, i, k, t)) in
         let+ t = elaborate_typ t in
         (i, t)
    in
    let env = assoc |> List.to_seq |> TypAliases.of_seq in
    let env = env |> TypAliases.map (Typ.subst_rec env) in
    let* env =
      env |> TypAliases.bindings
      |> MList.traverse (MPair.traverse return Typ.infer_and_resolve)
      >>- (List.to_seq >>> TypAliases.of_seq)
    in
    get_as TypAliases.field (TypAliases.union (fun _ v _ -> Some v) env)
  | `Include (at', p) ->
    let inc_path = Path.coalesce at' p |> Path.ensure_ext Path.inc_ext in
    let* env, newer =
      (ImportChain.with_path at' inc_path
      <<< TypIncludes.get_or_put inc_path
      <<< Elab.modularly)
        (let* env =
           Fetch.fetch at' inc_path
           >>= Parser.parse_utf_8 Grammar.typ_defs Lexer.offside ~path:inc_path
           >>= elaborate_defs
         in
         Annot.Typ.resolve Kind.resolve >> get Annot.field >>= MVar.get
         >>- fun annot -> (env, annot))
    in
    let* annot = get Annot.field in
    MVar.mutate annot (Annot.LocMap.merge Map.prefer_lhs newer)
    >> get_as TypAliases.field @@ TypAliases.merge Map.prefer_lhs env

and elaborate_typ = function
  | `Mu (at', t) -> elaborate_typ t >>- fun t -> `Mu (at', t)
  | `Const (_, _) as inn -> return inn
  | `Var (_, i) as inn -> (
    let* t_opt = TypAliases.find_opt i in
    match t_opt with
    | None -> return inn
    | Some t ->
      let t = Typ.freshen t in
      Annot.Typ.use i (Typ.at t) >> return t)
  | `Lam (at', i, k, t) ->
    avoid i @@ fun i ->
    elaborate_typ t |> Typ.VarMap.adding i k >>- fun t -> `Lam (at', i, k, t)
  | `App (at', f, x) ->
    let+ f = elaborate_typ f and+ x = elaborate_typ x in
    `App (at', f, x)
  | `ForAll (at', t) -> elaborate_typ t >>- fun t -> `ForAll (at', t)
  | `Exists (at', t) -> elaborate_typ t >>- fun t -> `Exists (at', t)
  | `Arrow (at', d, c) ->
    let+ d = elaborate_typ d and+ c = elaborate_typ c in
    `Arrow (at', d, c)
  | `Product (at', ls) ->
    FomAST.Row.traverse elaborate_typ ls >>- fun ls -> `Product (at', ls)
  | `Sum (at', ls) ->
    FomAST.Row.traverse elaborate_typ ls >>- fun ls -> `Sum (at', ls)
  | `LetDefIn (_, def, e) ->
    let* typ_aliases = elaborate_def def in
    TypAliases.setting typ_aliases (elaborate_typ e)
  | `Import (at', p) ->
    let sig_path = Path.coalesce at' p |> Path.ensure_ext Path.sig_ext in
    (ImportChain.with_path at' sig_path
    <<< TypImports.get_or_put sig_path
    <<< Elab.modularly)
      (Fetch.fetch at' sig_path
      >>= Parser.parse_utf_8 Grammar.typ_exp Lexer.offside ~path:sig_path
      >>= elaborate_typ >>= Typ.infer_and_resolve >>- Typ.ground)

and elaborate_defs = function
  | [] -> get TypAliases.field
  | def :: defs ->
    let* typ_aliases = elaborate_def def in
    TypAliases.setting typ_aliases (elaborate_defs defs)

let maybe_annot e tO =
  match tO with
  | None -> return e
  | Some t ->
    let+ t = elaborate_typ t in
    let at = Typ.at t in
    let i = Exp.Var.fresh at in
    `App (at, `Lam (at, i, t, `Var (at, i)), e)

let rec elaborate = function
  | `Const (at, c) ->
    Exp.Const.traverse_typ elaborate_typ c >>- fun c -> `Const (at, c)
  | `Var _ as ast -> return ast
  | `Lam (at, i, t, e) | `LamPat (at, `Id (_, i, t), e) ->
    let+ t = elaborate_typ t and+ e = elaborate e in
    `Lam (at, i, t, e)
  | `App (at, f, x) ->
    let+ f = elaborate f and+ x = elaborate x in
    `App (at, f, x)
  | `Gen (at, i, k, e) ->
    avoid i @@ fun i ->
    elaborate e |> Typ.VarMap.adding i k >>- fun e -> `Gen (at, i, k, e)
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
    TypAliases.setting typ_aliases (elaborate e)
  | `Mu (at, e) -> elaborate e >>- fun e -> `Mu (at, e)
  | `IfElse (at, c, t, e) ->
    let+ c = elaborate c and* t = elaborate t and+ e = elaborate e in
    `IfElse (at, c, t, e)
  | `Product (at, fs) ->
    FomAST.Row.traverse elaborate fs >>- fun fs -> `Product (at, fs)
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
    avoid ti @@ fun ti ->
    let+ e = elaborate e |> Typ.VarMap.adding ti @@ Kind.fresh at in
    `UnpackIn (at, ti, ei, v, e)
  | `LetPat (at, `Pack (_, `Id (_, ei, _), ti, _), tO, v, e) ->
    let* v = elaborate v in
    let* v = maybe_annot v tO in
    avoid ti @@ fun ti ->
    let+ e = elaborate e |> Typ.VarMap.adding ti @@ Kind.fresh at in
    `UnpackIn (at, ti, ei, v, e)
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
    let e = elaborate_pat (`Var (at, i)) e p in
    let+ e = elaborate e in
    `Lam (at, i, t, e)
  | `LetPat (at, p, tO, v, e) ->
    let* v = elaborate v in
    let* v = maybe_annot v tO in
    let i = Exp.Var.fresh (FomCST.Exp.Pat.at p) in
    let e = elaborate_pat (`Var (at, i)) e p in
    let+ e = elaborate e in
    `LetIn (at, i, v, e)
  | `Annot (at, e, t) ->
    let+ e = elaborate e and+ t = elaborate_typ t in
    let x = Exp.Var.fresh at in
    `App (at, `Lam (at, x, t, `Var (at, x)), e)
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
      ImportChain.with_path at' sig_path
        ((TypImports.get_or_put sig_path <<< Elab.modularly)
           (Fetch.fetch at' sig_path
           >>= Parser.parse_utf_8 Grammar.typ_exp Lexer.offside ~path:sig_path
           >>= elaborate_typ >>= Typ.infer_and_resolve >>- Typ.ground)
        |> try_in (fun contents -> return @@ Some contents) @@ function
           | `Error_file_doesnt_exist (_, path) when path = sig_path ->
             return None
           | e -> fail e)
    in
    let* id, _, _, _ =
      (ImportChain.with_path at' mod_path
      <<< ExpImports.get_or_put mod_path
      <<< Elab.modularly)
        (let* ast =
           Fetch.fetch at' mod_path
           >>= Parser.parse_utf_8 Grammar.program Lexer.offside ~path:mod_path
           >>= elaborate >>- FomAST.Exp.initial_exp
         in
         let id = FomAST.Exp.Var.fresh at' in
         let ast =
           match typ_opt with
           | None -> ast
           | Some typ -> `App (at', `Lam (at', id, typ, `Var (at', id)), ast)
         in
         let* typ =
           Parameters.taking_in ast >>= Exp.infer >>= Parameters.result_without
         in
         let+ parameters = Parameters.get () in
         (id, ast, typ, parameters))
    in
    Parameters.add mod_path >> return @@ `Var (at', id)

(* *)

let elaborate_typ x = elaborate_typ x |> Error.generalize

(* *)

let elaborate cst =
  Elab.modularly
    (let* ast = elaborate cst >>- FomAST.Exp.initial_exp in
     let* typ =
       Parameters.taking_in ast >>= Exp.infer >>= Parameters.result_without
     in
     let+ parameters = Parameters.get () in
     (ast, typ, parameters))
  |> Error.generalize
