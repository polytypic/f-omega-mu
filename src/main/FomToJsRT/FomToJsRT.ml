open Js_of_ocaml
open FomBasis
open FomPPrint
open FomSource
open FomParser

let stringify = Js.Unsafe.pure_js_expr "JSON.stringify"
let to_js_string ?(max_width = 80) doc = to_string ~max_width doc |> Js.string

module JsType = struct
  let function' : Js.js_string Js.t Js.constr =
    Js.Unsafe.pure_js_expr "Function"

  let array' : Js.js_string Js.t Js.constr = Js.Unsafe.pure_js_expr "Array"
  let object' : Js.js_string Js.t Js.constr = Js.Unsafe.pure_js_expr "Object"
end

module JsHashtbl = Hashtbl.Make (struct
  type t = unit Js.t

  let equal = ( == )
  let hash = Hashtbl.hash
end)

let format max_width (value : unit Js.t) =
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
  let check_fuel fuel k = if 0 < fuel then k (fuel - 1) else utf8string "..." in
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
          if label = FomCST.Aggr.cons then
            match classify (get_value value) with
            | `Object value -> (
              match Js.object_keys value |> Js.to_array |> Array.to_list with
              | [key_x; key_xs] as keys when is_tuple keys ->
                as_aggr (fuel - 1) (Js.Unsafe.get value key_xs) >>- fun xs ->
                Js.Unsafe.get value key_x :: xs
              | _ -> zero)
            | _ -> zero
          else if label = FomCST.Aggr.nil && is_undefined (get_value value) then
            return []
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
              match Js.to_string (Js.Unsafe.get value (Js.string "name")) with
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

let () = Js.Unsafe.global##.format := format
