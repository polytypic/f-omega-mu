open Rea
open StdlibPlus
open FomPPrint
open FomSource
open FomDiag

(* *)

let () = Hashtbl.randomize ()

(* *)

module Options = struct
  let max_width = ref 80
  let whole = ref true
  let stop : [`Typ | `Js | `Run | `Eval] ref = ref `Run
end

(* *)

exception HttpError of (int * Cohttp.Code.meth * Uri.t)

let of_lwt op =
  suspend @@ fun resume ->
  match try Ok (op ()) with e -> Error e with
  | Ok p ->
    Lwt.on_any p (fun x -> resume @@ `Ok x) (fun e -> resume @@ `Error e)
  | Error e -> resume @@ `Error e

let error_io at exn = fail @@ `Error_io (at, exn)

let fetch at uri =
  if FomElab.Path.is_http uri then
    of_lwt (fun () ->
        let open Lwt.Syntax in
        let open Cohttp in
        let open Cohttp_lwt_unix in
        let uri = Uri.of_string uri in
        let* resp, body = Client.get uri in
        let code = resp |> Response.status |> Code.code_of_status in
        if 200 <= code && code < 300 then Cohttp_lwt.Body.to_string body
        else Lwt.fail @@ HttpError (code, `GET, uri))
    |> map_error @@ function
       | HttpError (404, _, _) -> `Error_file_doesnt_exist (at, uri)
       | exn -> `Error_io (at, exn)
  else
    of_lwt (fun () ->
        let open Lwt.Syntax in
        let* channel = Lwt_io.open_file ~mode:Lwt_io.input uri in
        Lwt.finalize
          (fun () -> Lwt_io.read channel)
          (fun () -> Lwt_io.close channel))
    |> map_error @@ function
       | Unix.Unix_error (Unix.ENOENT, _, _) ->
         `Error_file_doesnt_exist (at, uri)
       | exn -> `Error_io (at, exn)

let with_process_write at cmd args op =
  let* out =
    of_lwt (fun () -> Lwt_process.open_process_out (cmd, args) |> Lwt.return)
    |> handle (error_io at)
  in
  let write s =
    of_lwt (fun () -> Lwt_io.write out#stdin s) |> handle (error_io at)
  and flush =
    of_lwt (fun () -> Lwt_io.flush out#stdin) |> handle (error_io at)
  in
  op write flush
  >> (of_lwt (fun () -> Lwt_io.close out#stdin) |> handle (error_io at))
  >> (of_lwt (fun () -> out#status)
     |> tryin (error_io at) @@ function
        | Unix.WEXITED 0 -> unit
        | Unix.WEXITED c ->
          error_io at @@ Failure ("Process exited with code " ^ Int.to_string c)
        | Unix.WSIGNALED s ->
          error_io at @@ Failure ("Process killed by signal " ^ Int.to_string s)
        | Unix.WSTOPPED s ->
          error_io at @@ Failure ("Process stopped by signal " ^ Int.to_string s)
     )

let doc msg default = "    (default: " ^ default ^ ")\n\n    " ^ msg ^ "\n"

let repl_js =
  {|'use strict'
require('repl').start({prompt: '', terminal: false, ignoreUndefined: true})
|}

let () =
  let files = ref [] in
  Arg.parse
    [
      ( "-max-width",
        Arg.Int
          (fun w ->
            if 0 = w || (20 <= w && w <= 200) then Options.max_width := w),
        doc "Set maximum width for various outputs." "80" );
      ( "-stop",
        Arg.Symbol
          ( ["type"; "js"; "run"; "eval"],
            function
            | "type" -> Options.stop := `Typ
            | "js" -> Options.stop := `Js
            | "run" -> Options.stop := `Run
            | _ -> Options.stop := `Eval ),
        doc "Stop after specified IL/output has been computed and output it."
          "run" );
      ( "-whole",
        Arg.Bool (( := ) Options.whole),
        doc "Whole program compilation." "true" );
    ]
    (fun file -> files := file :: !files)
    (Filename.basename Sys.executable_name
    ^ " [options] <file.fom>\n\nOptions:\n");
  let at = Loc.of_path (Sys.getcwd () ^ "/.")
  and max_width = !Options.max_width
  and whole = !Options.whole
  and stop = !Options.stop
  and files = List.rev !files in
  let p, r = Lwt.wait () in
  let elab uri = FomElab.elaborate @@ `Import (at, JsonString.of_utf8 uri) in
  (match stop with
  | `Typ ->
    List.iter_er
      (fun uri ->
        let+ _, typ, _ = elab uri in
        typ |> FomPP.Typ.pp |> to_string ~max_width |> Printf.printf "%s\n")
      files
  | `Js ->
    List.iter_er
      (fun uri ->
        let* ast, _, paths = elab uri in
        FomToJsC.to_js ~whole ~top:`Top ast paths >>- Printf.printf "%s\n")
      files
  | `Run ->
    with_process_write at "node" [|"-"|] @@ fun write flush ->
    write repl_js
    >> (fetch at "docs/prelude.js" >>= write)
    >> List.iter_er
         (fun uri ->
           let* ast, _, paths = elab uri in
           let* js = FomToJsC.to_js ~whole ~top:`Body ast paths in
           write ";(() => " >> write js >> write ")()\n" >> flush)
         files
  | `Eval ->
    with_process_write at "node" [|"-"|] @@ fun write flush ->
    write repl_js
    >> (fetch at "docs/FomToJsRT.js" >>= write)
    >> (fetch at "docs/prelude.js" >>= write)
    >> List.iter_er
         (fun uri ->
           let* ast, _, paths = elab uri in
           let* js = FomToJsC.to_js ~whole ~top:`Body ast paths in
           write ";console.log(format("
           >> write @@ Int.to_string max_width
           >> write ", (() => " >> write js >> write ")()))\n" >> flush)
         files)
  |> handle (fun error ->
         let+ diagnostic = Diagnostic.of_error error in
         diagnostic |> Diagnostic.pp |> to_string ~max_width
         |> Printf.eprintf "%s\n";
         exit 1)
  >>- Lwt.wakeup r
  |> Tailrec.spawn
       (object
          inherit [_] Tailrec.async
          inherit [_, _, _] FomEnv.Env.empty ()
          method! fetch at path = fetch at path
       end);
  Lwt_main.run p
