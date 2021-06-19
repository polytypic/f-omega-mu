open Js_of_ocaml
open FomBasis
open FomPP
open FomSource
open FomAST
open FomDiag
open FomParser
open FomChecker
open FomEnv
open FomToJs

(* *)

let () = Hashtbl.randomize ()

(* *)

exception HttpError of (int * Cohttp.Code.meth * Uri.t)

let of_lwt op =
  Rea.of_async @@ fun r on_error on_ok ->
  match try Ok (op r) with e -> Error e with
  | Ok p -> Lwt.on_any p on_ok on_error
  | Error e -> on_error e

let fetch at filename =
  let open Rea in
  of_lwt (fun _ ->
      let open Lwt.Syntax in
      let open Cohttp in
      let open Cohttp_lwt_jsoo in
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

(* *)

let to_js_string ?(max_width = 80) doc = to_string ~max_width doc |> Js.string

let js_pos pos =
  object%js
    val line = pos.Lexing.pos_lnum - 1
    val ch = Pos.column_of pos - 1
  end

let js_token begins ends name =
  object%js
    val begins = begins
    val ends = ends
    val name = Js.string name
  end

let js_loc (begins, ends) =
  object%js
    val file = Js.string begins.Lexing.pos_fname
    val begins = js_pos begins
    val ends = js_pos ends
  end

(* *)

let let_type_mu = utf8string "let type " ^^ mu_lower
let and_mu = break_0_0 ^^ utf8string "and " ^^ mu_lower

let pp_typ ?(is_alias = false) t =
  let open FomChecker.Typ in
  let pp_typ t =
    let typ_doc = Typ.pp ~pp_annot:(Fun.const empty) t in
    match Typ.hanging t with
    | Some (sep, _) -> sep ^^ typ_doc
    | None -> break_1 ^^ typ_doc |> nest 2 |> group
  in
  let m, _ = collect_mus_closed IdSet.empty t TypSet.empty in
  let n = TypSet.cardinal m in
  let decon = function
    | `Mu (_, `Lam (_, i, _, t)) -> (i, t)
    | _ -> failwith "impossible"
  in
  if
    n = 0
    || (is_alias && n = 1 && TypSet.mem t m)
    || n
       <> (m |> TypSet.to_seq
          |> Seq.map (decon >>> fst)
          |> IdSet.of_seq |> IdSet.cardinal)
  then
    pp_typ t
  else
    let ds = TypSet.elements m in
    let t = replace_closed_mus m t in
    let ds =
      ds
      |> List.map @@ fun mu ->
         let i, t = decon mu in
         let t = replace_closed_mus m t in
         Id.pp i ^^ space_equals ^^ pp_typ t
    in
    nest 2 (pp_typ t)
    ^^ break_0
    ^^ nest 2
         (break_0
         ^^
         match ds with
         | d :: ds -> let_type_mu ^^ d ^^ concat (ds |> List.map (( ^^ ) and_mu))
         | _ -> failwith "impossible")

(* *)

let js_use_def ?(max_width = 60) (def, o) =
  object%js
    val def = js_loc def
    val uses = o#uses.contents |> List.map js_loc |> Array.of_list |> Js.array

    val annot =
      (match o#annot with
      | `Label (id, typ) -> Label.pp id ^^ colon ^^ pp_typ typ
      | `ExpId (id, typ) -> Exp.Id.pp id ^^ colon ^^ pp_typ typ
      | `TypAlias (id, typ) ->
        Typ.Id.pp id ^^ space_equals ^^ pp_typ ~is_alias:true typ
      | `TypId (id, kind) ->
        group (Typ.Id.pp id ^^ colon ^^ nest 2 (break_1 ^^ Kind.pp kind)))
      |> to_js_string ~max_width
  end

module JsType = struct
  let function' : Js.js_string Js.t Js.constr =
    Js.Unsafe.pure_js_expr "Function"

  let array' : Js.js_string Js.t Js.constr = Js.Unsafe.pure_js_expr "Array"
  let object' : Js.js_string Js.t Js.constr = Js.Unsafe.pure_js_expr "Object"
end

let stringify = Js.Unsafe.pure_js_expr "JSON.stringify"

module Cb : sig
  type 'a t

  val invoke : 'a t -> 'a Js.t -> ('r, 'e, unit) Rea.t
end = struct
  type 'a t = unit

  let invoke fn x =
    Rea.delay @@ fun () ->
    Js.Unsafe.fun_call fn [|Js.Unsafe.inject x|] |> ignore;
    Rea.unit
end

module JsHashtbl = Hashtbl.Make (struct
  type t = unit Js.t

  let equal = ( == )
  let hash = Hashtbl.hash
end)

let js_codemirror_mode =
  object%js
    method format (value : unit Js.t) max_width =
      let known = JsHashtbl.create 100 in
      let rec format_object obj =
        let keys = Js.object_keys obj |> Js.to_array |> Array.to_list in
        if
          keys
          |> List.map (fun s ->
                 (Js.to_string s |> Label.of_string Loc.dummy, ()))
          |> Tuple.is_tuple
        then
          keys
          |> List.map (Js.Unsafe.get obj >>> format)
          |> separate comma_break_1 |> egyptian parens 2
        else
          keys
          |> List.map (fun key ->
                 utf8string (Js.to_string key)
                 ^^ space_equals_space
                 ^^ format (Js.Unsafe.get obj key)
                 |> group)
          |> separate comma_break_1 |> egyptian braces 2
      and format_array array =
        utf8string (Js.to_string (Js.Unsafe.get array 0))
        ^^ space_equals_space
        ^^ format (Js.Unsafe.get array 1)
        |> egyptian brackets 2
      and format value =
        match JsHashtbl.find_opt known value with
        | None ->
          let n = JsHashtbl.length known + 1 in
          let used = ref false in
          JsHashtbl.replace known value (n, used);
          let result =
            match Js.typeof value |> Js.to_string with
            | "object" | "undefined" ->
              if Js.instanceof value JsType.array' then
                format_array value
              else if Js.instanceof value JsType.object' then
                format_object value
              else
                utf8string "()"
            | "number" ->
              let float = Js.Unsafe.coerce value |> Js.float_of_number in
              utf8format "%.16g" float
            | "boolean" ->
              if Js.Unsafe.coerce value |> Js.to_bool then true' else false'
            | "string" ->
              utf8string
                (Js.Unsafe.fun_call stringify [|Js.Unsafe.inject value|]
                |> Js.to_string)
            | "function" ->
              if Js.instanceof value JsType.array' then
                format_array value
              else if Js.instanceof value JsType.function' then
                let name =
                  match
                    Js.to_string (Js.Unsafe.get value (Js.string "name"))
                  with
                  | "" -> underscore
                  | name -> utf8string name
                in
                lambda_lower ^^ name
              else
                format_object value
            | _ -> utf8string "unknown"
          in
          JsHashtbl.remove known value;
          if !used then
            group (mu_lower ^^ utf8format "α_%d" n ^^ dot ^^ result)
          else
            result
        | Some (n, used) ->
          used := true;
          utf8format "α_%d" n
      in
      format value |> to_js_string ~max_width

    method check path input max_width (on_result : _ Cb.t) =
      let open Rea in
      let path = Js.to_string path in
      let env = Env.empty ~fetch () in
      let def_uses () =
        env#annotations |> Hashtbl.to_seq
        |> Seq.map (js_use_def ~max_width)
        |> Array.of_seq |> Js.array
      in
      Js.to_string input
      |> Parser.parse_utf_8 Grammar.program Lexer.plain ~path
      >>= FomElab.elaborate >>= FomElab.with_modules >>= Exp.infer >>- pp_typ
      >>- (fun t -> utf8string "type:" ^^ t)
      >>- to_js_string ~max_width
      |> try_in
           (fun typ ->
             Cb.invoke on_result
             @@ object%js
                  val typ = typ
                  val defUses = def_uses ()
                  val diagnostics = Js.array [||]
                end)
           (fun error ->
             let diagnostics = Error.to_diagnostics error in
             Cb.invoke on_result
             @@ object%js
                  val typ =
                    match diagnostics with
                    | (loc, overview), [] ->
                      Loc.pp loc ^^ colon ^^ break_1 ^^ overview
                      |> nest 2 |> group |> to_js_string ~max_width
                    | (_, overview), details ->
                      overview ^^ colon ^^ break_0
                      ^^ nest 2
                           (break_0
                           ^^ separate break_0_0
                                (details
                                |> List.map @@ fun (loc, msg) ->
                                   Loc.pp loc ^^ colon ^^ break_1 ^^ msg
                                   |> nest 2 |> group))
                      |> to_js_string ~max_width

                  val defUses = def_uses ()

                  val diagnostics =
                    match diagnostics with
                    | first, rest ->
                      first :: rest |> Array.of_list
                      |> Array.map (fun ((begins, ends), msg) ->
                             object%js
                               val begins = js_pos begins
                               val ends = js_pos ends
                               val message = msg |> to_js_string ~max_width
                             end)
                      |> Js.array
                end)
      |> start env

    method compile path input (on_result : _ Cb.t) =
      let open Rea in
      input |> Js.to_string
      |> Parser.parse_utf_8 Grammar.program Lexer.plain
           ~path:(Js.to_string path)
      >>= FomElab.elaborate >>= FomElab.with_modules >>= to_js >>- Js.string
      |> try_in (Cb.invoke on_result) (fun _ ->
             Cb.invoke on_result @@ Js.string "")
      |> start (Env.empty ~fetch ())

    method token input =
      try
        let {Lexer.begins; ends; name} =
          input |> Js.to_string |> Lexer.token_info_utf_8
        in
        js_token begins ends name
      with _ -> js_token 0 0 "error"
  end

let () = Js.Unsafe.global##.fom := js_codemirror_mode
