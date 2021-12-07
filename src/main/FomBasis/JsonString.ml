open Exn.Syntax
open Fun.Syntax

type t = string

let compare = String.compare

(* *)

let of_utf8_json = Fun.id
let to_utf8_json = Fun.id

(* *)

let of_utf8 str =
  let buffer = Buffer.create (String.length str * 2) in
  let encoder = Uutf.encoder `UTF_8 @@ `Buffer buffer in
  let encode c = Uutf.encode encoder @@ `Uchar c |> ignore in
  let to_hex i =
    Uchar.of_int
      (i
      +
      if 0 <= i && i <= 9 then Uchar.to_int (Uchar.of_char '0')
      else Uchar.to_int (Uchar.of_char 'a') - 10)
  in
  encode @@ Uchar.of_char '"';
  str
  |> Uutf.String.fold_utf_8
       (fun i _ -> function
         | `Malformed _ ->
           failwithf "Malformed UTF-8 in string at char index %d" i
         | `Uchar u ->
           let c = Uchar.to_int u in
           if (0x0000 <= c && c <= 0x001f) || (0x007f <= c && c <= 0x009f) then (
             encode @@ Uchar.of_char '\\';
             if u = Uchar.of_char '\b' then encode @@ Uchar.of_char 'b'
             else if u = Uchar.of_int 0x0c then encode @@ Uchar.of_char 'f'
             else if u = Uchar.of_char '\n' then encode @@ Uchar.of_char 'n'
             else if u = Uchar.of_char '\r' then encode @@ Uchar.of_char 'r'
             else if u = Uchar.of_char '\t' then encode @@ Uchar.of_char 't'
             else (
               encode @@ Uchar.of_char 'u';
               encode @@ to_hex ((c lsr 12) land 0xf);
               encode @@ to_hex ((c lsr 8) land 0xf);
               encode @@ to_hex ((c lsr 4) land 0xf);
               encode @@ to_hex ((c lsr 0) land 0xf)))
           else if Uchar.of_char '"' = u || Uchar.of_char '\\' = u then (
             encode @@ Uchar.of_char '\\';
             encode u)
           else encode u;
           i + 1)
       0
  |> ignore;
  encode @@ Uchar.of_char '"';
  Uutf.encode encoder `End |> ignore;
  Buffer.contents buffer

let to_utf8 lit =
  let buffer = Buffer.create (String.length lit * 2) in
  let encoder = Uutf.encoder `UTF_8 @@ `Buffer buffer in
  let hex_to_int h c =
    (h lsl 4)
    lor
    if Uchar.of_char '0' <= c && c <= Uchar.of_char '9' then
      Uchar.to_int c - Uchar.to_int (Uchar.of_char '0')
    else if Uchar.of_char 'a' <= c && c <= Uchar.of_char 'f' then
      Uchar.to_int c - Uchar.to_int (Uchar.of_char 'a') + 10
    else Uchar.to_int c - Uchar.to_int (Uchar.of_char 'A') + 10
  in
  let is_white c =
    Uchar.of_char ' ' = c
    || Uchar.of_char '\t' = c
    || Uchar.of_char '\n' = c
    || Uchar.of_char '\r' = c
  in
  lit
  |> Uutf.String.fold_utf_8
       (fun (s, i) _ u ->
         let encode c =
           Uutf.encode encoder @@ `Uchar c |> ignore;
           (`Unescaped, i + 1)
         in
         match (s, u) with
         | `Unescaped, `Uchar c ->
           if Uchar.of_char '\\' = c then (`Escaped, i + 1)
           else if Uchar.of_char '"' = c then (`Unescaped, i + 1)
           else if Uchar.of_char '\n' = c || Uchar.of_char '\r' = c then (
             Uutf.encode encoder @@ `Uchar (Uchar.of_char '\n') |> ignore;
             (`Escaped, i + 1))
           else encode c
         | `Escaped, `Uchar c ->
           if
             Uchar.of_char '"' = c
             || Uchar.of_char '\\' = c
             || Uchar.of_char '/' = c
           then encode c
           else if Uchar.of_char 'b' = c then encode (Uchar.of_char '\b')
           else if Uchar.of_char 'f' = c then encode (Uchar.of_int 0x0c)
           else if Uchar.of_char 'n' = c then encode (Uchar.of_char '\n')
           else if Uchar.of_char 'r' = c then encode (Uchar.of_char '\r')
           else if Uchar.of_char 't' = c then encode (Uchar.of_char '\t')
           else if is_white c then (`Continued, i + 1)
           else (`Hex0, i + 1)
         | `Continued, `Uchar c ->
           ((if is_white c then `Continued else `Unescaped), i + 1)
         | `Hex0, `Uchar c -> (`Hex1 (hex_to_int 0 c), i + 1)
         | `Hex1 h, `Uchar c -> (`Hex2 (hex_to_int h c), i + 1)
         | `Hex2 h, `Uchar c -> (`Hex3 (hex_to_int h c), i + 1)
         | `Hex3 h, `Uchar c -> encode (Uchar.of_int (hex_to_int h c))
         | _, `Malformed _ ->
           failwithf "Malformed UTF-8 in string literal at char index %d" i)
       (`Unescaped, 0)
  |> ignore;
  Uutf.encode encoder `End |> ignore;
  Buffer.contents buffer

(* *)

let of_utf8_json_literal = of_utf8_json >>> to_utf8 >>> of_utf8

(* *)

let empty = of_utf8_json "\"\""
let is_empty x = x = empty
