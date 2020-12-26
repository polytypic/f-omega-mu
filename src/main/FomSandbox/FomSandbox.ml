open Js_of_ocaml
open FomBasis
open FomPP
open FomSource
open FomSyntax
open FomParser
open FomChecker
open FomToJs

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
    val begins = js_pos begins

    val ends = js_pos ends
  end

let js_use_def (def, o) =
  object%js
    val def = js_loc def

    val uses = o#uses.contents |> List.map js_loc |> Array.of_list |> Js.array

    val annot =
      (match o#annot with
      | `Label (id, typ) ->
        [Label.pp id; colon; [break_1; Typ.pp typ] |> concat |> nest 2]
        |> concat |> group
      | `ExpId (id, typ) ->
        [Exp.Id.pp id; colon; [break_1; Typ.pp typ] |> concat |> nest 2]
        |> concat |> group
      | `TypId (id, kind) ->
        [Typ.Id.pp id; colon; [break_1; Kind.pp kind] |> concat |> nest 2]
        |> concat |> group)
      |> to_js_string ~max_width:60
  end

module JsType = struct
  let function' : Js.js_string Js.t Js.constr =
    Js.Unsafe.pure_js_expr "Function"

  let array' : Js.js_string Js.t Js.constr = Js.Unsafe.pure_js_expr "Array"
end

let stringify = Js.Unsafe.pure_js_expr "JSON.stringify"

let js_codemirror_mode =
  object%js
    method format (value : unit Js.t) =
      let known = Hashtbl.create 100 in
      let rec format_object obj =
        let keys = Js.object_keys obj in
        keys |> Js.to_array |> Array.to_list
        |> List.map (fun key ->
               [
                 utf8string (Js.to_string key);
                 space_equals;
                 [break_1; format (Js.Unsafe.get obj key)] |> concat |> nest 1;
               ]
               |> concat |> group)
        |> separate comma_break_1 |> braces
      and format_array array =
        [
          utf8string (Js.to_string (Js.Unsafe.get array 0));
          space_equals;
          [break_1; format (Js.Unsafe.get array 1)] |> concat |> nest 1;
        ]
        |> concat |> brackets
      and format value =
        match Hashtbl.find_opt known value with
        | None ->
          let n = Hashtbl.length known + 1 in
          let used = ref false in
          Hashtbl.add known value (n, used);
          let result =
            match Js.typeof value |> Js.to_string with
            | "object" ->
              if Js.instanceof value JsType.array' then
                format_array value
              else
                format_object value
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
                [lambda_lower; name] |> concat
              else
                format_object value
            | _ -> utf8string "unknown"
          in
          Hashtbl.remove known value;
          if !used then
            [mu_lower; utf8format "α_%d" n; dot; break_0; result]
            |> concat |> nest 1 |> group
          else
            result
        | Some (n, used) ->
          used := true;
          utf8format "α_%d" n
      in
      format value |> to_js_string

    method check input =
      let env = Env.empty () in
      let def_uses () =
        env#annotations |> Hashtbl.to_seq |> Seq.map js_use_def |> Array.of_seq
        |> Js.array
      in
      try
        let typ =
          Js.to_string input
          |> parse_utf_8 Grammar.program Lexer.plain
          |> Exp.check |> Reader.run env |> Typ.pp |> to_js_string
        in
        object%js
          val typ = typ

          val defUses = def_uses ()

          val diagnostics = Js.array [||]
        end
      with exn ->
        object%js
          val typ =
            match exn with
            | Diagnostic.Error ((loc, overview), []) ->
              [Loc.pp loc; colon; break_1; overview]
              |> concat |> nest 2 |> group |> to_js_string
            | Diagnostic.Error ((_, overview), details) ->
              [
                [overview; colon; break_0] |> concat;
                [
                  break_0;
                  details
                  |> List.map (fun (loc, msg) ->
                         [Loc.pp loc; colon; break_1; msg]
                         |> concat |> nest 2 |> group)
                  |> separate (concat [break_0; break_0]);
                ]
                |> concat |> nest 2;
              ]
              |> concat |> to_js_string
            | Failure message -> Js.string message
            | exn -> Printexc.to_string exn |> Js.string

          val defUses = def_uses ()

          val diagnostics =
            match exn with
            | Diagnostic.Error (first, rest) ->
              first :: rest |> Array.of_list
              |> Array.map (fun ((begins, ends), msg) ->
                     object%js
                       val begins = js_pos begins

                       val ends = js_pos ends

                       val message = msg |> to_js_string
                     end)
              |> Js.array
            | _ -> Js.array [||]
        end

    method compile input =
      try
        Js.to_string input
        |> parse_utf_8 Grammar.program Lexer.plain
        |> to_js |> Js.string
      with _ -> Js.string ""

    method token input =
      try
        let {Lexer.begins; ends; name} =
          input |> Js.to_string |> Lexer.token_info_utf_8
        in
        js_token begins ends name
      with _ -> js_token 0 0 "error"
  end

let () = Js.Unsafe.global##.fom := js_codemirror_mode
