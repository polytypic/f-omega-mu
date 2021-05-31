open FomBasis
open FomDiag

(* *)

let () = Hashtbl.randomize ()

(* *)

module Options = struct
  let max_width = ref 80
  let stop : [`Typ | `Js | `Run] ref = ref `Run
end

(* *)

exception HttpError of (int * Cohttp.Code.meth * Uri.t)

let of_lwt op =
  Rea.of_async @@ fun r on_error on_ok ->
  match try Ok (op r) with e -> Error e with
  | Ok p -> Lwt.on_any p on_ok on_error
  | Error e -> on_error e

let fetch at filename =
  let open Rea in
  if FomElab.Path.is_http filename then
    of_lwt (fun _ ->
        let open Lwt.Syntax in
        let open Cohttp in
        let open Cohttp_lwt_unix in
        let uri = Uri.of_string filename in
        let* resp, body = Client.get uri in
        let code = resp |> Response.status |> Code.code_of_status in
        if 200 <= code && code < 300 then
          Cohttp_lwt.Body.to_string body
        else
          Lwt.fail @@ HttpError (code, `GET, uri))
    |> try_in return @@ function
       | HttpError (404, _, _) -> fail @@ `Error_file_doesnt_exist (at, filename)
       | exn -> fail @@ `Error_io (at, exn)
  else
    of_lwt (fun _ ->
        let open Lwt.Syntax in
        let* channel = Lwt_io.open_file ~mode:Lwt_io.input filename in
        Lwt.finalize
          (fun () -> Lwt_io.read channel)
          (fun () -> Lwt_io.close channel))
    |> try_in return @@ function
       | Unix.Unix_error (Unix.ENOENT, _, _) ->
         fail @@ `Error_file_doesnt_exist (at, filename)
       | exn -> fail @@ `Error_io (at, exn)

(* *)

let run_process_with_input command input =
  (* TODO: Use Lwt *)
  let out = Unix.open_process_out command in
  input |> List.iter (output_string out);
  flush out;
  match Unix.close_process_out out with
  | Unix.WEXITED exit_code when exit_code = 0 -> ()
  | _ -> failwith "Sub process didn't exit successfully"

let typ_includes = FomElab.TypIncludes.create ()
let typ_imports = FomElab.TypImports.create ()
let exp_imports = FomElab.ExpImports.create ()

let process filename =
  let open Rea in
  let at = FomSource.Loc.of_path (Sys.getcwd () ^ "/.") in
  let p = FomCST.LitString.of_utf8 filename in
  let cst = `Import (at, p) in
  let max_width = !Options.max_width in
  (let* ast, typ =
     cst |> FomElab.elaborate >>= FomElab.with_modules
     |> with_env
          (ignore
          >>> FomEnv.Env.empty ~fetch ~typ_includes ~typ_imports ~exp_imports)
   in
   (if !Options.stop = `Typ then (
      typ |> FomAST.Typ.pp |> FomPP.to_string ~max_width |> Printf.printf "%s\n";
      fail `Stop)
   else
     unit)
   >> let* js = ast |> FomToJs.to_js in
      (if !Options.stop = `Js then (
         js |> Printf.printf "%s\n";
         fail `Stop)
      else
        unit)
      >> let* prelude = fetch at "docs/prelude.js" in
         run_process_with_input "node" [prelude; ";\n"; js];
         unit)
  |> try_in return @@ function
     | `Stop -> unit
     | #Error.t as error ->
       let message =
         let open FomPP in
         match Error.to_diagnostics error with
         | (loc, overview), [] ->
           [
             [FomSource.Loc.pp loc; colon] |> concat;
             [break_1; break_0; overview] |> concat |> nest 2 |> group;
           ]
           |> concat |> to_string ~max_width
         | (loc, overview), details ->
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
       in
       Printf.printf "%s\n" message;
       exit 1

let () =
  let files = ref [] in
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
    (fun file -> files := file :: !files)
    (Filename.basename Sys.executable_name ^ " <file.fom>");
  let p, r = Lwt.wait () in
  let open Rea in
  !files |> List.rev |> MList.iter process
  >>- (fun () -> Lwt.wakeup r ())
  |> start ();
  Lwt_main.run p
