open FomBasis
open FomEnv

(* *)

open Cats

(* *)

let () = Hashtbl.randomize ()

(* *)

let typ_includes = FomElab.TypIncludes.create ()
let typ_imports = FomElab.TypImports.create ()
let exp_imports = FomElab.ExpImports.create ()

module Env = struct
  let empty ?(fetch = FomElab.Fetch.dummy) =
    Env.empty ~fetch ~typ_includes ~typ_imports ~exp_imports
end

let mods_simplified : (string, (Zero.t, FomToJs.Erased.t) LVar.t) Hashtbl.t =
  Hashtbl.create 100

let topological_deps paths =
  let added = Hashtbl.create 100 in
  let deps = ref [] in
  let rec loop path =
    if Hashtbl.mem added path then
      unit
    else
      let* (_, _, _, paths), _ = FomElab.ExpImports.get path in
      paths |> List.iter_fr loop >>- fun () ->
      if not (Hashtbl.mem added path) then (
        Hashtbl.replace added path ();
        deps := path :: !deps)
  in
  paths |> List.iter_fr loop >>- fun () -> !deps

let erase_and_simplify_all paths =
  paths
  |> List.map_fr @@ fun path ->
     let* (id, ast, _, _), _ = FomElab.ExpImports.get path in
     let+ erased =
       match Hashtbl.find_opt mods_simplified path with
       | Some var -> LVar.get var |> generalize_error
       | None ->
         let* var = LVar.create (ast |> FomToJs.erase |> FomToJs.simplify) in
         Hashtbl.replace mods_simplified path var;
         LVar.get var |> generalize_error
     in
     (id, path, erased)

let whole_program_to_js ast paths =
  paths |> topological_deps >>= erase_and_simplify_all
  >>- List.fold_left
        (fun prg (id, _, erased) -> `App (`Lam (id, prg), erased))
        (FomToJs.erase ast)
  >>= FomToJs.simplify >>= FomToJs.to_js >>- to_string

let mods_in_js : (string, (Zero.t, Cats.t) LVar.t) Hashtbl.t =
  Hashtbl.create 100

let compile_to_js_all paths =
  paths |> erase_and_simplify_all
  >>= List.map_fr @@ fun (id, path, erased) ->
      match Hashtbl.find_opt mods_in_js path with
      | Some var -> LVar.get var |> generalize_error
      | None ->
        let* var =
          LVar.create
            ( erased |> FomToJs.to_js ~top:(`Const id) >>- fun js ->
              str "// " ^ str path ^ str "\n" ^ js )
        in
        Hashtbl.replace mods_in_js path var;
        LVar.get var |> generalize_error

let modules_to_js ast paths =
  let* paths = topological_deps paths in
  let* modules = compile_to_js_all paths in
  let+ prg = ast |> FomToJs.erase |> FomToJs.simplify >>= FomToJs.to_js in
  modules
  |> List.fold_left (fun prg js -> js ^ str "\n\n" ^ prg) (str "// main\n" ^ prg)
  |> to_string
