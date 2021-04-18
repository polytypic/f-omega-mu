open FomBasis

module Options = struct
  let max_width = ref 80
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
  let max_width = !Options.max_width in
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
    compile
    (Filename.basename Sys.executable_name ^ " <file.fom>")
