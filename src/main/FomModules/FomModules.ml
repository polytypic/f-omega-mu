open FomBasis
open FomSource
open FomCST

let inc_ext = ".fomi"
let sig_ext = ".foms"
let mod_ext = ".fom"

let is_absolute filename =
  StringExt.is_prefix "/" filename || StringExt.is_prefix "https://" filename

let resolve loc lit ~ext =
  let filename = LitString.to_utf8 lit in
  let parent_dir = loc |> Loc.filename |> Filename.dirname in
  let filename =
    FilenameExt.canonic
      (if is_absolute filename then filename else parent_dir ^ "/" ^ filename)
  in
  if Filename.extension filename = ext then
    filename
  else
    filename ^ ext

(* *)

module FindDeps = struct
  open Conser

  let rec in_def = function
    | `Typ (_, _, _, t) -> in_typ t
    | `TypRec (_, bs) -> bs |> iter (snd >>> in_typ)
    | `Include _ as inc -> yield inc

  and in_typ = function
    | `Const (_, _) | `Var (_, _) -> return ()
    | `ForAll (_, t) | `Exists (_, t) | `Mu (_, t) | `Lam (_, _, _, t) ->
      in_typ t
    | `App (_, f, x) -> in_typ f >> in_typ x
    | `Arrow (_, d, c) -> in_typ d >> in_typ c
    | `Product (_, ls) | `Sum (_, ls) -> ls |> iter (snd >>> in_typ)
    | `LetDefIn (_, d, e) -> in_def d >> in_typ e

  let in_defs = iter in_def

  let rec in_pat = function
    | `Id (_, _, t) -> in_typ t
    | `Product (_, fs) ->
      fs |> iter (snd >>> function `Pat p -> in_pat p | `Ann t -> in_typ t)
    | `Pack (_, _, _, t) -> in_typ t

  let rec in_exp = function
    | `Const (_, c) -> iter in_typ @@ Exp.Const.collect_typ c
    | `Var _ -> return ()
    | `Target (_, t, _) -> in_typ t
    | `Lam (_, _, t, e) -> in_typ t >> in_exp e
    | `AppL (_, x, f) | `AppR (_, f, x) | `App (_, f, x) -> in_exp f >> in_exp x
    | `Pack (_, t, e, _) | `Annot (_, e, t) | `Inst (_, e, t) ->
      in_exp e >> in_typ t
    | `LetIn (_, _, v, e) -> in_exp v >> in_exp e
    | `Case (_, e)
    | `Select (_, e, _)
    | `Inject (_, _, e)
    | `Gen (_, _, _, e)
    | `Mu (_, e) ->
      in_exp e
    | `IfElse (_, c, t, e) -> in_exp c >> in_exp t >> in_exp e
    | `Product (_, fs) -> fs |> iter (snd >>> in_exp)
    | `UnpackIn (_, _, _, v, e) -> in_exp v >> in_exp e
    | `LetPatRec (_, pvs, e) ->
      pvs |> iter (fun (p, e) -> in_pat p >> in_exp e) >> in_exp e
    | `LamPat (_, p, e) -> in_pat p >> in_exp e
    | `LetPat (_, p, tO, v, e) ->
      in_pat p >> iter in_typ @@ Option.to_list tO >> in_exp v >> in_exp e
    | `LetDefIn (_, d, e) -> in_def d >> in_exp e
    | `Import _ as imp -> yield imp

  let run inn = inn >>> Conser.run >>> snd
end

let find_deps_defs = FindDeps.run FindDeps.in_defs
let find_deps_typ = FindDeps.run FindDeps.in_typ
let find_deps = FindDeps.run FindDeps.in_exp
