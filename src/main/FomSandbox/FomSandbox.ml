open Js_of_ocaml
open FomBasis
open FomPP
open FomSource
open FomAnnot
open FomDiag
open FomParser

(* *)

let () = Hashtbl.randomize ()

(* *)

exception HttpError of (int * Cohttp.Code.meth * Uri.t)

let of_lwt op =
  of_async @@ fun r on_error on_ok ->
  match try Ok (op r) with e -> Error e with
  | Ok p -> Lwt.on_any p on_ok on_error
  | Error e -> on_error e

let fetch at filename =
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

let js_token begins ends name state =
  object%js
    val begins = begins
    val ends = ends
    val name = Js.string name
    val state = state
  end

let js_loc (begins, ends) =
  object%js
    val file = Js.string begins.Lexing.pos_fname
    val begins = js_pos begins
    val ends = js_pos ends
  end

(* *)

let type_mu = utf8string "type " ^^ mu_lower
let and_mu = break_0_0 ^^ utf8string "and " ^^ mu_lower

let pp_typ t =
  let open Typ in
  let+ t = contract t in
  let pp_typ t =
    let typ_doc = pp ~pp_annot:(const empty) t in
    match hanging t with
    | Some (sep, _) -> sep ^^ typ_doc
    | None -> gnest 2 (break_1 ^^ typ_doc)
  in
  let m, _ = collect_mus_closed VarSet.empty t TypSet.empty in
  let n = TypSet.cardinal m in
  let decon = function
    | `Mu (_, `Lam (_, i, _, t)) -> (i, t)
    | _ -> failwith "decon"
  in
  if
    n = 0
    || n
       <> (m |> TypSet.to_seq
          |> Seq.map (decon >>> fst)
          |> VarSet.of_seq |> VarSet.cardinal)
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
         Var.pp i ^^ space_equals ^^ pp_typ t
    in
    nest 2 (pp_typ t)
    ^^ break_0
    ^^ nest 2
         (break_0
         ^^
         match ds with
         | d :: ds -> type_mu ^^ d ^^ concat (ds |> List.map (( ^^ ) and_mu))
         | _ -> failwith "pp_typ")

(* *)

let js_use_def ?(max_width = 60) (def, o) =
  let+ annot =
    (match o#annot with
    | `Label (id, typ) ->
      let+ typ = pp_typ typ in
      FomAST.Label.pp id ^^ colon ^^ typ
    | `ExpId (id, typ) ->
      let+ typ = pp_typ typ in
      FomAST.Exp.Var.pp id ^^ colon ^^ typ
    | `TypId (id, kind) ->
      return @@ gnest 2 (Typ.Var.pp id ^^ colon_break_1 ^^ FomAST.Kind.pp kind))
    >>- to_js_string ~max_width
  in
  object%js
    val def = js_loc def

    val uses =
      o#uses |> Annot.LocSet.elements |> List.map js_loc |> Array.of_list
      |> Js.array

    val annot = annot
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

  val invoke : 'a t -> 'a Js.t -> ('r, 'e, unit) rea
end = struct
  type 'a t = unit

  let invoke fn x =
    delay @@ fun () ->
    Js.Unsafe.fun_call fn [|Js.Unsafe.inject x|] |> ignore;
    unit
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
      let format_label value =
        match Js.typeof value |> Js.to_string with
        | "number" ->
          let float = Js.Unsafe.coerce value |> Js.float_of_number in
          utf8format "%.16g" float
        | _ -> utf8string @@ Js.to_string value
      in
      let rec format_object obj =
        let keys = Js.object_keys obj |> Js.to_array |> Array.to_list in
        if
          keys
          |> List.map (fun s ->
                 (Js.to_string s |> FomAST.Label.of_string Loc.dummy, ()))
          |> FomAST.Row.is_tuple
        then
          keys
          |> List.map (Js.Unsafe.get obj >>> format ~atomize:false)
          |> separate comma_break_1 |> egyptian parens 2
        else
          keys
          |> List.map (fun key ->
                 utf8string (Js.to_string key)
                 ^^ space_equals_space
                 ^^ format ~atomize:false (Js.Unsafe.get obj key)
                 |> group)
          |> separate comma_break_1 |> egyptian braces 2
      and format_array ~atomize array =
        let label = format_label (Js.Unsafe.get array 0) in
        let value = Js.Unsafe.get array 1 in
        match Js.typeof value |> Js.to_string with
        | "undefined" -> tick ^^ label
        | _ ->
          tick ^^ label ^^ space ^^ format ~atomize:true value
          |> if atomize then egyptian parens 2 else id
      and format ~atomize value =
        match JsHashtbl.find_opt known value with
        | None ->
          let n = JsHashtbl.length known + 1 in
          let used = ref false in
          JsHashtbl.replace known value (n, used);
          let result =
            match Js.typeof value |> Js.to_string with
            | "object" | "undefined" ->
              if Js.instanceof value JsType.array' then
                format_array ~atomize value
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
                format_array ~atomize value
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
            group (mu_lower ^^ alpha_lower ^^ subscript n ^^ dot ^^ result)
          else
            result
        | Some (n, used) ->
          used := true;
          alpha_lower ^^ subscript n
      in
      format ~atomize:false value |> to_js_string ~max_width

    method offset input i = Tokenizer.offset_as_utf_16 (Js.to_string input) i

    method check path input max_width (on_result : _ Cb.t) =
      Profiling.Counter.reset_all ();
      let path = Js.to_string path in
      let env = FomToJsC.Env.empty ~fetch () in
      let def_uses =
        Field.get Annot.field env |> MVar.get >>- Annot.LocMap.bindings
        >>= List.map_m (js_use_def ~max_width)
        >>- (Array.of_list >>> Js.array)
      in
      input |> Js.to_string
      |> Parser.parse_utf_8 Grammar.mods Lexer.offside ~path
      >>= FomElab.elaborate
      >>= (fun (_, t, deps) ->
            let+ t = pp_typ t in
            (utf8string "type:" ^^ t |> to_js_string ~max_width, deps))
      |> try_in
           (fun (typ, deps) ->
             Profiling.Counter.dump_all ();
             let* defUses = def_uses in
             Cb.invoke on_result
             @@ object%js
                  val typ = typ
                  val defUses = defUses

                  val dependencies =
                    deps |> Array.of_list |> Array.map Js.string |> Js.array

                  val diagnostics = Js.array [||]
                end)
           (fun error ->
             Profiling.Counter.dump_all ();
             let* defUses = def_uses in
             let diagnostics = Diagnostic.of_error error in
             Cb.invoke on_result
             @@ object%js
                  val typ =
                    match diagnostics with
                    | (loc, overview), [] ->
                      gnest 2 (Loc.pp loc ^^ colon_break_1 ^^ overview)
                      |> to_js_string ~max_width
                    | (_, overview), details ->
                      overview ^^ colon_break_1
                      ^^ nest 2
                           (break_0
                           ^^ (details
                              |> List.map (fun (loc, msg) ->
                                     gnest 2 (Loc.pp loc ^^ colon_break_1 ^^ msg))
                              |> separate break_0_0))
                      |> to_js_string ~max_width

                  val defUses = defUses
                  val dependencies = Js.array [||]

                  val diagnostics =
                    match diagnostics with
                    | first, rest ->
                      first :: rest |> Array.of_list
                      |> Array.map (fun ((begins, ends), msg) ->
                             object%js
                               val file = Js.string begins.Lexing.pos_fname
                               val begins = js_pos begins
                               val ends = js_pos ends
                               val message = msg |> to_js_string ~max_width
                             end)
                      |> Js.array
                end)
      |> start env

    method compile whole path input (on_result : _ Cb.t) =
      let path = Js.to_string path in
      input |> Js.to_string
      |> Parser.parse_utf_8 Grammar.mods Lexer.offside ~path
      >>= FomElab.elaborate
      >>= (fun (ast, _, paths) ->
            (if whole then
               FomToJsC.whole_program_to_js
            else
              FomToJsC.modules_to_js)
              ast paths)
      >>- Js.string
      |> try_in (Cb.invoke on_result) (fun _ ->
             Cb.invoke on_result @@ Js.string "")
      |> start (FomToJsC.Env.empty ~fetch ())

    method token input state =
      try
        let {Tokenizer.begins; ends; name; state} =
          input |> Js.to_string |> Tokenizer.token_info_utf_8 state
        in
        js_token begins ends name state
      with _ -> js_token 0 0 "error" state

    val initial = Tokenizer.State.initial
  end

let () = Js.Unsafe.global##.fom := js_codemirror_mode
