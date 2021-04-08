open FomBasis

let max_width = 80

module Options = struct
  let stop : [`Typ | `Js | `Run] ref = ref `Run
end

let read_file filename =
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

let compile filename =
  let exception Stop in
  let stop () = raise Stop in
  try
    let contents = read_file filename in
    let cst =
      contents
      |> FomParser.parse_utf_8 FomParser.Grammar.program FomParser.Lexer.plain
           ~filename
    in
    let env = FomEnv.Env.empty () in
    let ast = cst |> FomElab.elaborate |> Reader.run env in
    let typ = ast |> FomChecker.Exp.infer |> Reader.run env in
    if !Options.stop = `Typ then
      typ |> FomAST.Typ.pp |> FomPP.to_string ~max_width |> Printf.printf "%s\n"
      |> stop;
    let js = ast |> FomToJs.to_js in
    if !Options.stop = `Js then
      js |> Printf.printf "%s\n" |> stop;
    run_process_with_input "node" [read_file "docs/prelude.js"; ";\n"; js]
  with
  | Stop -> ()
  | exn ->
    let message =
      match exn with
      | FomSource.Diagnostic.Error ((loc, overview), []) ->
        let open FomPP in
        [FomSource.Loc.pp loc; colon; break_1; overview]
        |> concat |> nest 2 |> group |> to_string ~max_width
      | FomSource.Diagnostic.Error ((_, overview), details) ->
        let open FomPP in
        [
          [overview; colon; break_0] |> concat;
          [
            break_0;
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
      ( "-stop",
        Arg.Symbol
          ( ["type"; "js"; "run"],
            function
            | "type" -> Options.stop := `Typ
            | "js" -> Options.stop := `Js
            | _ -> Options.stop := `Run ),
        "\tStop after specified IL/output has been computed and output it" );
    ]
    compile
    (Filename.basename Sys.executable_name ^ " <file.fom>")
