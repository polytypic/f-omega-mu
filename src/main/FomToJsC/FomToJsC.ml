open FomBasis
open FomEnv

(* *)

open Rea
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

let mods_simplified : (string, (Zero.t, FomToJs.Erased.t) IVar.t) Hashtbl.t =
  Hashtbl.create 100

let topological_deps paths =
  let added = Hashtbl.create 100 in
  let deps = ref [] in
  let rec loop path =
    if Hashtbl.mem added path then
      unit
    else
      let* _, _, _, paths = FomElab.ExpImports.get path in
      paths |> MList.iter loop >>- fun () ->
      if not (Hashtbl.mem added path) then (
        Hashtbl.replace added path ();
        deps := path :: !deps)
  in
  paths |> MList.iter loop |> FomElab.Error.generalize >>- fun () -> !deps

let erase_and_simplify_all paths =
  paths
  |> MList.traverse @@ fun path ->
     let* id, ast, _, _ = FomElab.ExpImports.get path in
     let+ erased =
       match Hashtbl.find_opt mods_simplified path with
       | Some var -> IVar.get var |> generalize_error
       | None ->
         let var = IVar.empty () in
         Hashtbl.replace mods_simplified path var;
         ast |> FomToJs.erase |> FomToJs.simplify |> catch >>= IVar.put var
         >> IVar.get var |> generalize_error
     in
     (id, path, erased)

let whole_program_to_js ast paths =
  paths |> topological_deps >>= erase_and_simplify_all
  >>- List.fold_left
        (fun prg (id, _, erased) -> `App (`Lam (id, prg), erased))
        (FomToJs.erase ast)
  >>= FomToJs.simplify >>= FomToJs.to_js >>- to_string

let mods_in_js : (string, (Zero.t, Cats.t) IVar.t) Hashtbl.t =
  Hashtbl.create 100

let compile_to_js_all paths =
  paths |> erase_and_simplify_all
  >>= MList.traverse @@ fun (id, path, erased) ->
      let+ js =
        match Hashtbl.find_opt mods_in_js path with
        | Some var -> IVar.get var |> generalize_error
        | None ->
          let var = IVar.empty () in
          Hashtbl.replace mods_in_js path var;
          erased
          |> FomToJs.to_js ~top:(`Const id)
          >>- (fun js -> str "// " ^ str path ^ str "\n" ^ js)
          |> catch >>= IVar.put var >> IVar.get var |> generalize_error
      in
      js

let modules_to_js ast paths =
  let* paths = topological_deps paths in
  let* modules = compile_to_js_all paths in
  let+ prg = ast |> FomToJs.erase |> FomToJs.simplify >>= FomToJs.to_js in
  modules
  |> List.fold_left (fun prg js -> js ^ str "\n\n" ^ prg) (str "// main\n" ^ prg)
  |> to_string
