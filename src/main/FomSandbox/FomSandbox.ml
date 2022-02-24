open Js_of_ocaml
open FomBasis
open FomPPrint
open FomSource
open FomAnnot
open FomDiag
open FomParser

(* *)

let () = Hashtbl.randomize ()

(* *)

exception HttpError of (int * Cohttp.Code.meth * Uri.t)

let of_lwt op =
  of_async @@ fun on_error on_ok ->
  match try Ok (op ()) with e -> Error e with
  | Ok p -> Lwt.on_any p on_ok on_error
  | Error e -> on_error e

let fetch at filename =
  of_lwt (fun () ->
      let open Lwt.Syntax in
      let open Cohttp in
      let open Cohttp_lwt_jsoo in
      let uri = Uri.of_string filename in
      let* resp, body = Client.get uri in
      let code = resp |> Response.status |> Code.code_of_status in
      if 200 <= code && code < 300 then Cohttp_lwt.Body.to_string body
      else Lwt.fail @@ HttpError (code, `GET, uri))
  |> map_error @@ function
     | HttpError (404, _, _) -> `Error_file_doesnt_exist (at, filename)
     | exn -> `Error_io (at, exn)

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
  then pp_typ t
  else
    let ds = TypSet.elements m in
    let t = replace_closed_mus m t in
    let ds =
      ds
      |> List.map @@ fun mu ->
         let i, t = decon mu in
         let t = replace_closed_mus m t in
         FomPP.Typ.Var.pp i ^^ space_equals ^^ pp_typ t
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
    | `Label (i, t) -> pp_typ t >>- fun t -> FomAST.Label.pp i ^^ colon ^^ t
    | `ExpId (i, t) -> pp_typ t >>- fun t -> FomAST.Exp.Var.pp i ^^ colon ^^ t
    | `TypId (i, k) ->
      return @@ gnest 2 (Typ.Var.pp i ^^ colon_break_1 ^^ FomPP.Kind.pp k))
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
  type t

  val invoke : t -> Js.Unsafe.any -> ('r, 'e, unit) rea
end = struct
  type t = unit

  let invoke fn x =
    delay @@ fun () ->
    Js.Unsafe.fun_call fn [|x|] |> ignore;
    unit
end

module JsHashtbl = Hashtbl.Make (struct
  type t = unit Js.t

  let equal = ( == )
  let hash = Hashtbl.hash
end)

let js_codemirror_mode =
  let env = FomEnv.Env.empty ~fetch () in
  object%js
    method format (value : unit Js.t) max_width =
      let known = JsHashtbl.create 100 in
      let string_label value =
        match Js.typeof value |> Js.to_string with
        | "number" ->
          let float = Js.Unsafe.coerce value |> Js.float_of_number in
          Printf.sprintf "%.16g" float
        | _ ->
          let s = Js.to_string value in
          if Lexer.is_id_or_nat s then s
          else s |> JsonString.of_utf8 |> JsonString.to_utf8_json
      in
      let format_label value = string_label value |> utf8string in
      let check_fuel fuel k =
        if 0 < fuel then k (fuel - 1) else utf8string "..."
      in
      let get_label sum = Js.Unsafe.get sum 0
      and get_value sum = Js.Unsafe.get sum 1 in
      let classify value =
        match Js.typeof value |> Js.to_string with
        | "object" ->
          if Js.instanceof value JsType.array' then `Array value
          else if Js.instanceof value JsType.object' then `Object value
          else `Unknown
        | "undefined" -> `Undefined
        | "number" -> `Number value
        | "boolean" -> `Boolean value
        | "string" -> `String value
        | "function" ->
          if Js.instanceof value JsType.array' then `Array value
          else if Js.instanceof value JsType.function' then `Function value
          else `Object value
        | _ -> `Unknown
      in
      let is_undefined value =
        Js.typeof value |> Js.to_string |> ( = ) "undefined"
      in
      let is_tuple keys =
        keys
        |> List.map (fun s ->
               (Js.to_string s |> FomAST.Label.of_string Loc.dummy, ()))
        |> FomAST.Row.is_tuple
      in
      let rec format_object ~fuel obj =
        check_fuel fuel @@ fun fuel ->
        let keys = Js.object_keys obj |> Js.to_array |> Array.to_list in
        if is_tuple keys then
          keys
          |> List.map (Js.Unsafe.get obj >>> format ~atomize:false ~fuel)
          |> separate comma_break_1 |> egyptian parens 2
        else
          keys
          |> List.map (fun key ->
                 format_label key ^^ space_equals_space
                 ^^ format ~atomize:false ~fuel (Js.Unsafe.get obj key)
                 |> group)
          |> separate comma_break_1 |> egyptian braces 2
      and as_aggr ~fuel value =
        let known = JsHashtbl.create 1000 in
        let rec as_aggr fuel value =
          if JsHashtbl.mem known value || fuel <= 0 then zero
          else (
            JsHashtbl.replace known value ();
            match classify value with
            | `Array value ->
              let label = get_label value |> string_label in
              if label = FomCST.Exp.cons then
                match classify (get_value value) with
                | `Object value -> (
                  match
                    Js.object_keys value |> Js.to_array |> Array.to_list
                  with
                  | [key_x; key_xs] as keys when is_tuple keys ->
                    as_aggr (fuel - 1) (Js.Unsafe.get value key_xs)
                    >>- fun xs -> Js.Unsafe.get value key_x :: xs
                  | _ -> zero)
                | _ -> zero
              else if label = FomCST.Exp.nil && is_undefined (get_value value)
              then return []
              else zero
            | _ -> zero)
        in
        as_aggr fuel value
      and format_array ~atomize ~fuel array =
        check_fuel fuel @@ fun fuel ->
        let label = format_label (get_label array) in
        let value = get_value array in
        if is_undefined value then tick ^^ label
        else
          tick ^^ label ^^ space ^^ format ~atomize:true ~fuel value
          |> if atomize then egyptian parens 2 else id
      and format ~atomize ~fuel value =
        check_fuel fuel @@ fun fuel ->
        match JsHashtbl.find_opt known value with
        | None -> (
          match as_aggr ~fuel value |> Option.run with
          | Some xs ->
            xs
            |> List.map (format ~atomize:false ~fuel)
            |> separate comma_break_1 |> egyptian brackets 2
          | None ->
            let n = JsHashtbl.length known + 1 in
            let used = ref false in
            JsHashtbl.replace known value (n, used);
            let result =
              match classify value with
              | `Array value -> format_array ~atomize ~fuel value
              | `Object value -> format_object ~fuel value
              | `Undefined -> utf8string "()"
              | `Number value ->
                let float = Js.Unsafe.coerce value |> Js.float_of_number in
                utf8format "%.16g" float
              | `Boolean value ->
                if Js.Unsafe.coerce value |> Js.to_bool then true' else false'
              | `String value ->
                utf8string
                  (Js.Unsafe.fun_call stringify [|Js.Unsafe.inject value|]
                  |> Js.to_string)
              | `Function value ->
                let name =
                  match
                    Js.to_string (Js.Unsafe.get value (Js.string "name"))
                  with
                  | "" -> underscore
                  | name -> utf8string name
                in
                lambda_lower ^^ name
              | `Unknown -> utf8string "unknown"
            in
            JsHashtbl.remove known value;
            if !used then
              group (mu_lower ^^ alpha_lower ^^ subscript n ^^ dot ^^ result)
            else result)
        | Some (n, used) ->
          used := true;
          alpha_lower ^^ subscript n
      in
      format ~atomize:false ~fuel:500 value |> to_js_string ~max_width

    method offset16 input i = Tokenizer.offset_as_utf_16 (Js.to_string input) i
    method offset32 input i = Tokenizer.offset_as_utf_32 (Js.to_string input) i

    method build whole path input max_width (on_elab : Cb.t) (on_pass : Cb.t)
        (on_fail : Cb.t) (on_js : Cb.t) =
      Profiling.Counter.reset_all ();
      let path = Js.to_string path in
      let def_uses =
        read Annot.field >>- Annot.LocMap.bindings
        >>= List.map_m (js_use_def ~max_width)
        >>- (Array.of_list >>> Js.array)
      in
      input |> Js.to_string
      |> Parser.parse_utf_8 Grammar.mods Lexer.offside ~path
      >>= FomElab.elaborate
      |> try_in
           (fun (ast, typ, deps) ->
             Profiling.Counter.dump_all ();
             let* () = Cb.invoke on_elab @@ Js.Unsafe.inject () in
             let* typ = pp_typ typ and* def_uses in
             Cb.invoke on_pass @@ Js.Unsafe.inject
             @@ object%js
                  val typ = utf8string "type:" ^^ typ |> to_js_string ~max_width
                  val defUses = def_uses

                  val dependencies =
                    deps |> Array.of_list |> Array.map Js.string |> Js.array

                  val diagnostics = Js.array [||]
                end
             >> FomToJsC.to_js ~whole ast deps
             |> try_in
                  (Js.string >>> Js.Unsafe.inject >>> Cb.invoke on_js)
                  (fun _ -> Cb.invoke on_js @@ Js.Unsafe.inject @@ Js.string ""))
           (fun error ->
             Profiling.Counter.dump_all ();
             let* () = Cb.invoke on_elab @@ Js.Unsafe.inject () in
             let* defUses = def_uses in
             let* diagnostics = Diagnostic.of_error error in
             Cb.invoke on_fail @@ Js.Unsafe.inject
             @@ object%js
                  val typ = Diagnostic.pp diagnostics |> to_js_string ~max_width
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
      |> Annot.scoping |> start env

    method synonyms =
      Tokenizer.synonyms |> Array.of_list
      |> Array.map (fun s ->
             object%js
               val unicode = Js.string s#unicode
               val ascii = Js.string s#ascii
               val bop = s#bop
             end)
      |> Js.array

    method identifiers =
      Js.to_string >>> Tokenizer.identifiers >>> Seq.map Js.string
      >>> Array.of_seq >>> Js.array

    val pervasives =
      Tokenizer.pervasives |> Array.of_list |> Array.map Js.string |> Js.array

    val keywords =
      Tokenizer.keywords |> Array.of_list |> Array.map Js.string |> Js.array

    method token input state =
      try
        let {Tokenizer.begins; ends; name; state} =
          input |> Js.to_string |> Tokenizer.token_info_utf_8 state
        in
        js_token begins ends name state
      with _ -> js_token 0 0 "error" state

    val initial = Tokenizer.State.initial

    method distancesCompare l r =
      EditDistance.compare (Js.to_array l) (Js.to_array r)

    method distancesUnrelated d = EditDistance.are_unrelated (Js.to_array d)

    method distances pat txt pat_uc txt_uc =
      let to_array = Js.to_string >>> UTF.UTF8.to_uchar_array in
      EditDistance.distances ~pat:(to_array pat) ~txt:(to_array txt)
        ~pat_uc:(to_array pat_uc) ~txt_uc:(to_array txt_uc)
      |> Js.array
  end

let () = Js.Unsafe.global##.fom := js_codemirror_mode
