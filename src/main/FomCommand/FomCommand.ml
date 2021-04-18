open FomBasis
open FomDiag

module Options = struct
  let max_width = ref 80
  let stop : [`Typ | `Js | `Run] ref = ref `Run
end

let read_file at filename =
  if not (Sys.file_exists filename) then
    Error.file_doesnt_exist at filename;
  let in_ch = open_in_bin filename in
  let size = in_channel_length in_ch in
  let contents = really_input_string in_ch size in
  close_in in_ch;
  contents

let run_process_with_input command input =
  let out = Unix.open_process_out command in
  input |> List.iter (output_string out);
  flush out;
  match Unix.close_process_out out with
  | Unix.WEXITED exit_code when exit_code = 0 -> ()
  | _ -> failwith "Sub process didn't exit successfully"

module PathMap = Map.Make (String)

let includeMap : FomAST.Typ.t FomAST.Typ.Env.t FomCST.Typ.IncludeMap.t ref =
  ref FomCST.Typ.IncludeMap.empty

let importMap : FomAST.Exp.Id.t FomCST.Exp.ImportMap.t ref =
  ref FomCST.Exp.ImportMap.empty

let moduleMap : (FomCST.Exp.t * FomCST.Typ.t option) FomAST.Exp.Env.t ref =
  ref FomAST.Exp.Env.empty

let rec process_includes include_chain at p =
  let inc_filename = FomModules.resolve at p ~ext:FomModules.inc_ext in
  PathMap.find_opt inc_filename include_chain
  |> Option.iter (Error.cyclic_includes at inc_filename);
  if not (FomCST.Typ.IncludeMap.mem inc_filename !includeMap) then (
    let include_chain = PathMap.add inc_filename at include_chain in
    let contents = read_file at inc_filename in
    let cst =
      contents
      |> FomParser.parse_utf_8 FomParser.Grammar.typ_defs FomParser.Lexer.plain
           ~filename:inc_filename
    in
    cst |> FomModules.find_deps_defs
    |> List.iter (fun (`Include (at, p)) -> process_includes include_chain at p);
    let env =
      FomEnv.Env.empty () |> fun r -> r#map_includes (Fun.const !includeMap)
    in
    let env = cst |> FomElab.elaborate_defs |> Reader.run env in
    includeMap := FomCST.Typ.IncludeMap.add inc_filename env !includeMap)

let rec process_imports imported import_chain at p program =
  let mod_filename = FomModules.resolve at p ~ext:FomModules.mod_ext in
  PathMap.find_opt mod_filename import_chain
  |> Option.iter (Error.cyclic_imports at mod_filename);
  let import_chain = PathMap.add mod_filename at import_chain in
  let id =
    match FomCST.Exp.ImportMap.find_opt mod_filename !importMap with
    | None ->
      let cst =
        read_file at mod_filename
        |> FomParser.parse_utf_8 FomParser.Grammar.program FomParser.Lexer.plain
             ~filename:mod_filename
      in
      let typ =
        let sig_filename =
          Filename.remove_extension mod_filename ^ FomModules.sig_ext
        in
        if Sys.file_exists sig_filename then (
          let cst =
            read_file at sig_filename
            |> FomParser.parse_utf_8 FomParser.Grammar.typ_exp
                 FomParser.Lexer.plain ~filename:sig_filename
          in
          cst |> FomModules.find_deps_typ
          |> List.iter (fun (`Include (at, p)) ->
                 process_includes PathMap.empty at p);
          Some cst)
        else
          None
      in
      let id = FomAST.Exp.Id.fresh at in
      importMap := FomCST.Exp.ImportMap.add mod_filename id !importMap;
      moduleMap := FomAST.Exp.Env.add id (cst, typ) !moduleMap;
      id
    | Some id -> id
  in
  if FomAST.Exp.IdSet.mem id imported then
    (imported, program)
  else
    let imported = FomAST.Exp.IdSet.add id imported in
    let cst, typ = FomAST.Exp.Env.find id !moduleMap in
    cst |> FomModules.find_deps
    |> List.fold_left
         (fun (imported, program) -> function
           | `Include (at, p) ->
             process_includes PathMap.empty at p;
             (imported, program)
           | `Import (at, p) ->
             process_imports imported import_chain at p program)
         ( imported,
           `LetPat (at, `Id (at, id, FomCST.Typ.zero at), typ, cst, program) )

let process filename =
  let at = FomSource.Loc.of_filename (Sys.getcwd () ^ "/.") in
  let p = FomCST.LitString.of_utf8 filename in
  let cst = `Import (at, p) in
  let max_width = !Options.max_width in
  let exception Stop in
  let stop () = raise Stop in
  try
    let _, cst =
      process_imports FomAST.Exp.IdSet.empty PathMap.empty at p cst
    in
    let env =
      FomEnv.Env.empty () |> fun r ->
      r#map_includes (Fun.const !includeMap) |> fun r ->
      r#map_imports (Fun.const !importMap)
    in
    let ast = cst |> FomElab.elaborate |> Reader.run env in
    let typ = ast |> FomChecker.Exp.infer |> Reader.run env in
    if !Options.stop = `Typ then
      typ |> FomAST.Typ.pp |> FomPP.to_string ~max_width |> Printf.printf "%s\n"
      |> stop;
    let js = ast |> FomToJs.to_js in
    if !Options.stop = `Js then
      js |> Printf.printf "%s\n" |> stop;
    run_process_with_input "node" [read_file at "docs/prelude.js"; ";\n"; js]
  with
  | Stop -> ()
  | exn ->
    let open FomPP in
    let message =
      match exn with
      | FomSource.Diagnostic.Error ((loc, overview), []) ->
        [
          [FomSource.Loc.pp loc; colon] |> concat;
          [break_1; break_0; overview] |> concat |> nest 2 |> group;
        ]
        |> concat |> to_string ~max_width
      | FomSource.Diagnostic.Error ((loc, overview), details) ->
        [
          [FomSource.Loc.pp loc; colon] |> concat;
          [break_1; break_0; overview; colon] |> concat |> nest 2 |> group;
          [
            [break_0; break_0] |> concat;
            details
            |> List.map (fun (loc, msg) ->
                   [FomSource.Loc.pp loc; colon; break_1; msg]
                   |> concat |> nest 2 |> group)
            |> separate (concat [break_0; break_0]);
          ]
          |> concat |> nest 2;
        ]
        |> concat |> to_string ~max_width
      | Failure message -> message
      | exn -> Printexc.to_string exn
    in
    Printf.printf "%s\n" message;
    exit 1

let () =
  Arg.parse
    [
      ( "-max-width",
        Arg.Int
          (fun w ->
            if 20 <= w && w <= 200 then
              Options.max_width := w),
        "\tSet maximum width for various outputs" );
      ( "-stop",
        Arg.Symbol
          ( ["type"; "js"; "run"],
            function
            | "type" -> Options.stop := `Typ
            | "js" -> Options.stop := `Js
            | _ -> Options.stop := `Run ),
        "\tStop after specified IL/output has been computed and output it" );
    ]
    process
    (Filename.basename Sys.executable_name ^ " <file.fom>")
