open FomBasis
open FomSource
open FomError
open FomDiag

(* *)

let () = Hashtbl.randomize ()

(* *)

module Options = struct
  let max_width = ref 80
  let whole = ref true
  let stop : [`Typ | `Js | `Run] ref = ref `Run
end

(* *)

exception HttpError of (int * Cohttp.Code.meth * Uri.t)

let of_lwt op =
  of_async @@ fun on_error on_ok ->
  match try Ok (op ()) with e -> Error e with
  | Ok p -> Lwt.on_any p on_ok on_error
  | Error e -> on_error e

let error_io at exn = fail @@ `Error_io (at, exn)

let fetch at filename =
  if FomElab.Path.is_http filename then
    of_lwt (fun () ->
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
    of_lwt (fun () ->
        let open Lwt.Syntax in
        let* channel = Lwt_io.open_file ~mode:Lwt_io.input filename in
        Lwt.finalize
          (fun () -> Lwt_io.read channel)
          (fun () -> Lwt_io.close channel))
    |> try_in return @@ function
       | Unix.Unix_error (Unix.ENOENT, _, _) ->
         fail @@ `Error_file_doesnt_exist (at, filename)
       | exn -> error_io at exn

(* *)

let run_process_with_input at command input =
  of_lwt (fun () ->
      Lwt_process.with_process_out command @@ fun out ->
      let open Lwt.Syntax in
      let rec write_input = function
        | [] ->
          let* () = Lwt_io.flush out#stdin in
          Lwt_io.close out#stdin
        | line :: lines ->
          let* () = Lwt_io.write out#stdin line in
          write_input lines
      in
      let* () = write_input input in
      out#status)
  |> try_in
       (function
         | Unix.WEXITED 0 -> unit
         | Unix.WEXITED c ->
           error_io at @@ Failure ("Process exited with code " ^ Int.to_string c)
         | Unix.WSIGNALED s ->
           error_io at @@ Failure ("Process killed by signal " ^ Int.to_string s)
         | Unix.WSTOPPED s ->
           error_io at
           @@ Failure ("Process stopped by signal " ^ Int.to_string s))
       (error_io at)

let process filename =
  let at = Loc.of_path (Sys.getcwd () ^ "/.") in
  let p = JsonString.of_utf8 filename in
  let env = FomToJsC.Env.empty ~fetch () in
  let cst = `Import (at, p) in
  let max_width = !Options.max_width in
  (let* ast, typ, paths = FomElab.elaborate cst |> replace_env env in
   (if !Options.stop = `Typ then (
      typ |> FomAST.Typ.pp |> FomPP.to_string ~max_width |> Printf.printf "%s\n";
      fail `Stop)
   else
     unit)
   >> let* js =
        (if !Options.whole then
           FomToJsC.whole_program_to_js
        else
          FomToJsC.modules_to_js)
          ast paths
        |> replace_env env
      in
      (if !Options.stop = `Js then (
         js |> Printf.printf "%s\n";
         fail `Stop)
      else
        unit)
      >> let* prelude = fetch at "docs/prelude.js" in
         run_process_with_input at ("node", [|"-"|]) [prelude; ";\n"; js])
  |> try_in return @@ function
     | `Stop -> unit
     | #Error.t as error ->
       error |> Diagnostic.of_error |> Diagnostic.pp
       |> FomPP.to_string ~max_width |> Printf.printf "%s\n";
       exit 1

let doc msg default = "    (default: " ^ default ^ ")\n\n    " ^ msg ^ "\n"

let () =
  let files = ref [] in
  Arg.parse
    [
      ( "-max-width",
        Arg.Int
          (fun w ->
            if 20 <= w && w <= 200 then
              Options.max_width := w),
        doc "Set maximum width for various outputs." "80" );
      ( "-stop",
        Arg.Symbol
          ( ["type"; "js"; "run"],
            function
            | "type" -> Options.stop := `Typ
            | "js" -> Options.stop := `Js
            | _ -> Options.stop := `Run ),
        doc "Stop after specified IL/output has been computed and output it."
          "run" );
      ( "-whole",
        Arg.Bool (( := ) Options.whole),
        doc "Whole program compilation." "true" );
    ]
    (fun file -> files := file :: !files)
    (Filename.basename Sys.executable_name
    ^ " [options] <file.fom>\n\nOptions:\n");
  let p, r = Lwt.wait () in
  !files |> List.rev |> List.iter_fr process
  >>- (fun () -> Lwt.wakeup r ())
  |> start ();
  Lwt_main.run p
