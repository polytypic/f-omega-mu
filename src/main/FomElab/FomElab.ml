open FomBasis
open FomSource
open FomParser
open FomAST
open FomAnnot
open FomDiag

(* *)

open Rea

module Path = struct
  let inc_ext = ".fomd"
  let sig_ext = ".fomt"
  let mod_ext = ".fom"

  (* *)

  let is_absolute filename = StringExt.is_prefix "/" filename

  let ensure_ext ext filename =
    if Filename.extension filename = ext then
      filename
    else
      filename ^ ext

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

  let is_http filename =
    StringExt.is_prefix "https://" filename
    || StringExt.is_prefix "http://" filename

  let resolve loc lit =
    let filename = LitString.to_utf8 lit in
    (if is_http filename then
       filename |> split_to_origin_and_path
    else
      loc |> Loc.filename |> Filename.dirname |> split_to_origin_and_path
      |> Pair.map Fun.id @@ fun parent_dir ->
         if is_absolute filename then
           filename
         else
           parent_dir ^ "/" ^ filename)
    |> Pair.map Fun.id FilenameExt.canonic
    |> join_origin_and_path
end

module Fetch = struct
  type e = [Error.file_doesnt_exist | Error.io_error]
  type t = Loc.t -> string -> (unit, e, string) Rea.t

  let field r = r#fetch

  class con (fetch : t) =
    object
      method fetch = fetch
    end

  let fetch at filename =
    invoke (fun r -> field r at filename) |> map_error (fun (#e as x) -> x)
end

module TypAliases = struct
  include FomAST.Typ.Env

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
  type t = [Error.io_error | Error.syntax_errors | Error.source_errors]

  let generalize x = map_error (fun (#t as x) -> x) x
end

module TypIncludes = struct
  type t = (string, (Error.t, FomAST.Typ.t FomAST.Typ.Env.t) IVar.t) Hashtbl.t

  let field r = r#typ_includes

  let get_or_put filename compute =
    VarTbl.get_or_put field filename compute |> Error.generalize

  class con (typ_includes : t) =
    object
      method typ_includes = typ_includes
    end
end

module TypImports = struct
  type t = (string, (Error.t, FomAST.Typ.t) IVar.t) Hashtbl.t

  let field r = r#typ_imports

  let get_or_put filename compute =
    VarTbl.get_or_put field filename compute |> Error.generalize

  class con (typ_imports : t) =
    object
      method typ_imports = typ_imports
    end
end

module ExpImports = struct
  type t =
    ( string,
      (Error.t, FomAST.Exp.Id.t * FomAST.Exp.t * FomAST.Typ.t option) IVar.t )
    Hashtbl.t

  let field r = r#exp_imports

  let get_or_put filename compute =
    VarTbl.get_or_put field filename compute |> Error.generalize

  class con (exp_imports : t) =
    object
      method exp_imports = exp_imports
    end
end

module PathMap = Map.Make (String)

module ImportChain = struct
  type t = Loc.t PathMap.t

  let field r = r#import_chain

  let with_path at filename compute =
    let* include_chain = get field in
    PathMap.find_opt filename include_chain
    |> MOption.iter (fun previously_at ->
           fail @@ `Error_cyclic_includes (at, filename, previously_at))
    >> compute

  class con =
    object
      val import_chain : t = PathMap.empty

      method import_chain =
        Field.make import_chain (fun v -> {<import_chain = v>})
    end
end

let avoid at i inn =
  mapping TypAliases.field (TypAliases.remove i)
  @@ let* exists =
       get_as TypAliases.field
         (TypAliases.exists (fun _ t' -> Typ.is_free i t'))
     in
     if exists then
       let i' = Typ.Id.freshen i in
       let v' = `Var (at, i') in
       mapping TypAliases.field (TypAliases.add i' v') (inn i')
     else
       inn i

let rec type_of_pat_lam = function
  | `Id (_, _, t) -> t
  | `Product (at, fs) ->
    Typ.product at
      (fs
      |> List.map @@ Pair.map Fun.id
         @@ function `Pat p -> type_of_pat_lam p | `Ann t -> t)
  | `Pack (_, _, _, t) -> t

let rec elaborate_pat p' e' = function
  | `Id (at, i, _) -> `LetIn (at, i, p', e')
  | `Product (at, fs) ->
    fs |> List.rev
    |> List.fold_left
         (fun e' -> function
           | l, `Pat p ->
             let i =
               Exp.Id.freshen (Exp.Id.of_name (Label.at l) (Label.name l))
             in
             `LetIn
               (at, i, `Select (at, p', l), elaborate_pat (`Var (at, i)) e' p)
           | l, `Ann _ ->
             `LetIn
               ( at,
                 Exp.Id.of_name (Label.at l) (Label.name l),
                 `Select (at, p', l),
                 e' ))
         e'
  | `Pack (at, `Id (_, i, _), t, _) -> `UnpackIn (at, t, i, p', e')
  | `Pack (at, p, t, _) ->
    let i = Exp.Id.fresh (FomCST.Exp.Pat.at p) in
    `UnpackIn (at, t, i, p', elaborate_pat (`Var (at, i)) e' p)

let rec elaborate_def = function
  | `Typ (_, i, kO, t) ->
    let* t = elaborate_typ t in
    Annot.Typ.alias i t
    >>
    let t =
      match kO with
      | None -> t
      | Some k ->
        let at = Kind.at k in
        let i = Typ.Id.fresh at in
        `App (at, `Lam (at, i, k, `Var (at, i)), t)
    in
    get_as TypAliases.field (TypAliases.add i (Typ.set_at (Typ.Id.at i) t))
  | `TypRec (_, bs) ->
    let* assoc =
      bs
      |> MList.traverse @@ fun (i, k, t) ->
         let at = Typ.Id.at i in
         let t = `Mu (at, `Lam (at, i, k, t)) in
         let* t = elaborate_typ t in
         Annot.Typ.alias i t >> return (i, t)
    in
    let env = assoc |> List.to_seq |> TypAliases.of_seq in
    let* annot = env_as Annot.field in
    let replaced i t = Annot.Typ.use' i (Typ.at t) annot in
    let env = env |> TypAliases.map (Typ.subst_rec ~replaced env) in
    get_as TypAliases.field (TypAliases.union (fun _ v _ -> Some v) env)
  | `Include (at', p) ->
    let inc_filename = Path.resolve at' p |> Path.ensure_ext Path.inc_ext in
    let* env =
      ImportChain.with_path at' inc_filename
        (TypIncludes.get_or_put inc_filename
           (TypAliases.setting TypAliases.empty
              (Fetch.fetch at' inc_filename
              >>= parse_utf_8 Grammar.typ_defs Lexer.plain
                    ~filename:inc_filename
              >>= elaborate_defs)))
    in
    get_as TypAliases.field
    @@ TypAliases.merge
         (fun _ l r ->
           match (l, r) with
           | Some l, _ -> Some l
           | _, Some r -> Some r
           | _, _ -> None)
         env

and elaborate_typ = function
  | `Mu (at', t) ->
    let+ t = elaborate_typ t in
    `Mu (at', t)
  | `Const (at', c) -> return @@ `Const (at', c)
  | `Var (at', i) -> (
    let* t_opt = TypAliases.find_opt i in
    match t_opt with
    | None -> return @@ `Var (at', i)
    | Some t -> Annot.Typ.use i (Typ.at t) >> return t)
  | `Lam (at', i, k, t) ->
    avoid at' i @@ fun i ->
    let+ t = elaborate_typ t in
    `Lam (at', i, k, t)
  | `App (at', f, x) ->
    let+ f = elaborate_typ f and+ x = elaborate_typ x in
    `App (at', f, x)
  | `ForAll (at', t) ->
    let+ t = elaborate_typ t in
    `ForAll (at', t)
  | `Exists (at', t) ->
    let+ t = elaborate_typ t in
    `Exists (at', t)
  | `Arrow (at', d, c) ->
    let+ d = elaborate_typ d and+ c = elaborate_typ c in
    `Arrow (at', d, c)
  | `Product (at', ls) ->
    let+ ls = ls |> MList.traverse @@ MPair.traverse return elaborate_typ in
    `Product (at', ls)
  | `Sum (at', ls) ->
    let+ ls = ls |> MList.traverse @@ MPair.traverse return elaborate_typ in
    `Sum (at', ls)
  | `LetDefIn (_, def, e) ->
    let* typ_aliases = elaborate_def def in
    TypAliases.setting typ_aliases (elaborate_typ e)
  | `Import (at', p) ->
    let sig_filename = Path.resolve at' p |> Path.ensure_ext Path.sig_ext in
    ImportChain.with_path at' sig_filename
      (TypImports.get_or_put sig_filename
         (TypAliases.setting TypAliases.empty
            (Fetch.fetch at' sig_filename
            >>= parse_utf_8 Grammar.typ_exp Lexer.plain ~filename:sig_filename
            >>= elaborate_typ)))

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
    let i = Exp.Id.fresh at in
    `App (at, `Lam (at, i, t, `Var (at, i)), e)

let rec elaborate = function
  | `Const (at, c) ->
    let+ c = c |> Exp.Const.traverse_typ elaborate_typ in
    `Const (at, c)
  | `Var _ as ast -> return ast
  | `Target (at, t, s) ->
    let+ t = elaborate_typ t in
    `Target (at, t, s)
  | `Lam (at, i, t, e) | `LamPat (at, `Id (_, i, t), e) ->
    let+ t = elaborate_typ t and+ e = elaborate e in
    `Lam (at, i, t, e)
  | `App (at, f, x) ->
    let+ f = elaborate f and+ x = elaborate x in
    `App (at, f, x)
  | `Gen (at, i, k, e) ->
    avoid at i @@ fun i ->
    let+ e = elaborate e in
    `Gen (at, i, k, e)
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
  | `Mu (at, e) ->
    let+ e = elaborate e in
    `Mu (at, e)
  | `IfElse (at, c, t, e) ->
    let+ c = elaborate c and* t = elaborate t and+ e = elaborate e in
    `IfElse (at, c, t, e)
  | `Product (at, fs) ->
    let+ fs = fs |> MList.traverse @@ MPair.traverse return elaborate in
    `Product (at, fs)
  | `Select (at, e, l) ->
    let+ e = elaborate e in
    `Select (at, e, l)
  | `Inject (at, l, e) ->
    let+ e = elaborate e in
    `Inject (at, l, e)
  | `Case (at, cs) ->
    let+ cs = elaborate cs in
    `Case (at, cs)
  | `Pack (at, t, e, x) ->
    let+ t = elaborate_typ t and+ e = elaborate e and+ x = elaborate_typ x in
    `Pack (at, t, e, x)
  | `UnpackIn (at, ti, ei, v, e) ->
    let* v = elaborate v in
    avoid at ti @@ fun ti ->
    let+ e = elaborate e in
    `UnpackIn (at, ti, ei, v, e)
  | `LetPat (at, `Pack (_, `Id (_, ei, _), ti, _), tO, v, e) ->
    let* v = elaborate v in
    let* v = maybe_annot v tO in
    avoid at ti @@ fun ti ->
    let+ e = elaborate e in
    `UnpackIn (at, ti, ei, v, e)
  | `LetPatRec (at, pvs, e) ->
    let p = pvs |> List.map fst |> FomCST.Exp.Pat.tuple at in
    let v = pvs |> List.map snd |> FomCST.Exp.tuple at in
    elaborate @@ `LetPat (at, p, None, `Mu (at, `LamPat (at, p, v)), e)
  | `LamPat (at, p, e) ->
    let t = type_of_pat_lam p in
    let* t = elaborate_typ t in
    let i = Exp.Id.fresh (FomCST.Exp.Pat.at p) in
    let e = elaborate_pat (`Var (at, i)) e p in
    let+ e = elaborate e in
    `Lam (at, i, t, e)
  | `LetPat (at, p, tO, v, e) ->
    let* v = elaborate v in
    let* v = maybe_annot v tO in
    let i = Exp.Id.fresh (FomCST.Exp.Pat.at p) in
    let e = elaborate_pat (`Var (at, i)) e p in
    let+ e = elaborate e in
    `LetIn (at, i, v, e)
  | `Annot (at, e, t) ->
    let+ e = elaborate e and+ t = elaborate_typ t in
    let x = Exp.Id.fresh at in
    `App (at, `Lam (at, x, t, `Var (at, x)), e)
  | `AppL (at, x, f) | `AppR (at, f, x) ->
    let+ x = elaborate x and+ f = elaborate f in
    `App (at, f, x)
  | `Import (at', p) ->
    let mod_filename = Path.resolve at' p |> Path.ensure_ext Path.mod_ext in
    let sig_filename = Filename.remove_extension mod_filename ^ Path.sig_ext in
    let* typ_opt =
      ImportChain.with_path at' sig_filename
        (TypImports.get_or_put sig_filename
           (TypAliases.setting TypAliases.empty
              (Fetch.fetch at' sig_filename
              >>= parse_utf_8 Grammar.typ_exp Lexer.plain ~filename:sig_filename
              >>= elaborate_typ))
        |> try_in (fun contents -> return @@ Some contents) @@ function
           | `Error_file_doesnt_exist (_, filename) when filename = sig_filename
             ->
             return None
           | e -> fail e)
    in
    let+ id, _, _ =
      ImportChain.with_path at' mod_filename
        (ExpImports.get_or_put mod_filename
           (TypAliases.setting TypAliases.empty
              (let+ ast =
                 Fetch.fetch at' mod_filename
                 >>= parse_utf_8 Grammar.program Lexer.plain
                       ~filename:mod_filename
                 >>= elaborate
               in
               let id = FomAST.Exp.Id.fresh at' in
               (id, ast, typ_opt))))
    in
    `Var (at', id)

(* *)

let elaborate_defs x = elaborate_defs x |> Error.generalize
let elaborate_typ x = elaborate_typ x |> Error.generalize
let elaborate x = elaborate x |> Error.generalize

(* *)

let with_modules ast =
  let at = Exp.at ast in
  let* exp_imports = env_as ExpImports.field in
  let+ imports =
    Hashtbl.to_seq exp_imports |> Seq.map snd |> List.of_seq
    |> MList.traverse @@ fun var -> IVar.get var |> Error.generalize
  in
  imports
  |> List.sort (Compare.the (fun (id, _, _) -> id) Exp.Id.compare)
  |> List.rev
  |> List.fold_left
       (fun prg (id, ast, typ_opt) ->
         match typ_opt with
         | None -> `LetIn (at, id, ast, prg)
         | Some typ -> `App (at, `Lam (at, id, typ, prg), ast))
       ast
