open Rea
open StdlibPlus

(* *)

open Cats

(* *)

module ModSimplified = struct
  type 'R t = (string, ('R, nothing, FomToJs.Lam.t) Memo.t) Hashtbl.t

  let create () = Hashtbl.create 100
  let field r = r#mod_simplified

  let get_or_put path compute =
    let* (hashtbl : 'R t) = env_as field in
    match Hashtbl.find_opt hashtbl path with
    | None ->
      let* var = Memo.create compute in
      Hashtbl.replace hashtbl path var;
      Memo.eval var |> gen_error
    | Some var -> Memo.eval var |> gen_error

  class ['R] con (mod_simplified : 'R t) =
    object
      method mod_simplified = mod_simplified
    end
end

module ModInJs = struct
  type 'R t = (string, ('R, nothing, Cats.t) Memo.t) Hashtbl.t

  let create () = Hashtbl.create 100
  let field r = r#mod_in_js

  let get_or_put path compute =
    let* (hashtbl : 'R t) = env_as field in
    match Hashtbl.find_opt hashtbl path with
    | None ->
      let* var = Memo.create compute in
      Hashtbl.replace hashtbl path var;
      Memo.eval var |> gen_error
    | Some var -> Memo.eval var |> gen_error

  class ['R] con (mod_in_js : 'R t) =
    object
      method mod_in_js = mod_in_js
    end
end

let topological_deps paths =
  let added = Hashtbl.create 100 in
  let deps = ref [] in
  let rec loop path =
    if Hashtbl.mem added path then unit
    else
      let* (_, _, _, paths), _ = FomElab.ExpImports.get path in
      paths |> List.iter_er loop >>- fun () ->
      if not (Hashtbl.mem added path) then (
        Hashtbl.replace added path ();
        deps := path :: !deps)
  in
  paths |> List.iter_er loop >>- fun () -> !deps

let erase_and_simplify_all paths =
  paths
  |> List.map_er @@ fun path ->
     let* (id, ast, _, _), _ = FomElab.ExpImports.get path in
     let+ erased =
       ModSimplified.get_or_put path
         (eta'0 @@ fun () -> ast |> FomToJs.erase |> FomToJs.simplify)
     in
     (id, path, erased)

let whole_program_to_js ~top ast paths =
  let+ js =
    paths |> topological_deps >>= erase_and_simplify_all
    >>- List.fold_left
          (fun prg (id, _, erased) -> `App (`Lam (id, prg), erased))
          (FomToJs.erase ast)
    >>= FomToJs.simplify >>= FomToJs.to_js ~top
  in
  to_string @@ match top with `Top -> str "'use strict'; " ^ js | `Body -> js

let compile_to_js_all paths =
  paths |> erase_and_simplify_all
  >>= List.map_er @@ fun (id, path, erased) ->
      ModInJs.get_or_put path
        ( erased |> FomToJs.to_js ~top:`Body >>- fun js ->
          str "// " ^ str path ^ str "\n" ^ str "const "
          ^ FomToJs.Lam.Var.to_js id ^ str " = (() => " ^ js ^ str ")()" )

let modules_to_js ~top ast paths =
  let* paths = topological_deps paths in
  let* modules = compile_to_js_all paths in
  let+ prg = ast |> FomToJs.erase |> FomToJs.simplify >>= FomToJs.to_js ~top in
  let js =
    modules
    |> List.fold_left
         (fun prg js -> js ^ str "\n\n" ^ prg)
         (str "// main\n" ^ prg)
  in
  to_string
  @@ match top with `Top -> str "'use strict';\n\n" ^ js | `Body -> js

let to_js ~whole ~top =
  if whole then whole_program_to_js ~top else modules_to_js ~top
