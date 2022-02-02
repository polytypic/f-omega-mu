open FomBasis
open FomSource
open Grammar
open Buffer

exception Exn_lexeme of Loc.t * string

let return_from buffer tok =
  let lhs, rhs = Buffer.loc buffer in
  (tok, lhs, rhs)

let arrow_right = [%sedlex.regexp? 0x2192 (* → *)]
let diamond = [%sedlex.regexp? 0x25c7 (* ◇ *)]
let double_angle_quote_lhs = [%sedlex.regexp? 0x00ab (* « *)]
let double_angle_quote_rhs = [%sedlex.regexp? 0x00bb (* » *)]
let double_comma = [%sedlex.regexp? 0x201e (* „ *)]
let exists = [%sedlex.regexp? 0x2203 (* ∃ *)]
let for_all = [%sedlex.regexp? 0x2200 (* ∀ *)]
let greater_equal = [%sedlex.regexp? 0x2265 (* ≥ *)]
let lambda_lower = [%sedlex.regexp? 0x03bb (* λ *)]
let lambda_upper = [%sedlex.regexp? 0x039b (* Λ *)]
let less_equal = [%sedlex.regexp? 0x2264 (* ≤ *)]
let logical_and = [%sedlex.regexp? 0x2227 (* ∧ *)]
let logical_not = [%sedlex.regexp? 0x00ac (* ¬ *)]
let logical_or = [%sedlex.regexp? 0x2228 (* ∨ *)]
let mu_lower = [%sedlex.regexp? 0x03bc (* μ *)]
let not_equal = [%sedlex.regexp? 0x2260 (* ≠ *)]
let triangle_lhs = [%sedlex.regexp? 0x25c1 (* ◁ *)]
let triangle_rhs = [%sedlex.regexp? 0x25b7 (* ▷ *)]

(* *)

let comment = [%sedlex.regexp? "#", Star (Compl ('\n' | '\r'))]
let white = [%sedlex.regexp? Plus (Chars " \t\n\r")]
let nat_10 = [%sedlex.regexp? "0" | '1' .. '9', Star (Opt '_', Plus '0' .. '9')]

(* *)

let id_ex_encodings =
  [|
    (Uchar.of_char '!', [|0x001c3|] (* ǃ *));
    (Uchar.of_char '"', [|0x01425|] (* ᐥ *));
    (Uchar.of_char '(', [|0x1bc19|] (* 𛰙 *));
    (Uchar.of_char ')', [|0x1bc1a|] (* 𛰚 *));
    (Uchar.of_char '*', [|0x0156f|] (* ᕯ *));
    (Uchar.of_char '+', [|0x10601|] (* 𐘁 *));
    (Uchar.of_char ',', [|0x0a4f9|] (* ꓹ *));
    (Uchar.of_char '-', [|0x0172d|] (* ᜭ *));
    (Uchar.of_char '.', [|0x0a4f8|] (* ꓸ *));
    (Uchar.of_char '/', [|0x0a937|] (* ꤷ *));
    (Uchar.of_char ':', [|0x0a4fd|] (* ꓽ *));
    (Uchar.of_char ';', [|0x0a4fc|] (* ꓼ *));
    (Uchar.of_char '<', [|0x01438|] (* ᐸ *));
    (Uchar.of_char '=', [|0x0a60c|] (* ꘌ *));
    (Uchar.of_char '>', [|0x01433|] (* ᐳ *));
    (Uchar.of_char '[', [|0x16a47|] (* 𖩇 *));
    (Uchar.of_char '\'', [|0x0141f|] (* ᐟ *));
    (Uchar.of_char '\\', [|0x10458|] (* 𐑘 *));
    (Uchar.of_char ']', [|0x16a49|] (* 𖩉 *));
    (Uchar.of_char '`', [|0x01420|] (* ᐠ *));
    (Uchar.of_char '{', [|0x1bc1d|] (* 𛰝 *));
    (Uchar.of_char '|', [|0x001c0|] (* ǀ *));
    (Uchar.of_char '}', [|0x1bc1e|] (* 𛰞 *));
    (Uchar.of_int 0x2192, [|0x10664|] (* 𐙤 *));
    (Uchar.of_int 0x2200, [|0x0a4ef|] (* ꓯ *));
    (Uchar.of_int 0x2203, [|0x0a4f1|] (* ꓱ *));
    (Uchar.of_int 0x2227, [|0x01431|] (* ᐱ *));
    (Uchar.of_int 0x2228, [|0x0142f|] (* ᐯ *));
    (Uchar.of_int 0x2264, [|0x01438; 0x0a60c|] (* ᐸꘌ *));
    (Uchar.of_int 0x2265, [|0x01433; 0x0a60c|] (* ᐳꘌ *));
  |]
  |> Array.sorted (Compare.the fst Uchar.compare)
  |> Array.map
       (Pair.map id (Array.map Uchar.of_int >>> UTF.UTF8.of_uchar_array))

let id_ex =
  [%sedlex.regexp?
    ( 0x001c0 (* ǀ *)
    | 0x001c3 (* ǃ *)
    | 0x0141f (* ᐟ *)
    | 0x01420 (* ᐠ *)
    | 0x01425 (* ᐥ *)
    | 0x0142f (* ᐯ *)
    | 0x01431 (* ᐱ *)
    | 0x01433 (* ᐳ *)
    | 0x01438 (* ᐸ *)
    | 0x0156f (* ᕯ *)
    | 0x0172d (* ᜭ *)
    | 0x0a4ef (* ꓯ *)
    | 0x0a4f1 (* ꓱ *)
    | 0x0a4f8 (* ꓸ *)
    | 0x0a4f9 (* ꓹ *)
    | 0x0a4fc (* ꓼ *)
    | 0x0a4fd (* ꓽ *)
    | 0x0a60c (* ꘌ *)
    | 0x0a937 (* ꤷ *)
    | 0x10601 (* 𐘁 *)
    | 0x10664 (* 𐙤 *)
    | 0x16a47 (* 𖩇 *)
    | 0x16a49 (* 𖩉 *)
    | 0x1bc19 (* 𛰙 *)
    | 0x1bc1a (* 𛰚 *)
    | 0x1bc1d (* 𛰝 *)
    | 0x1bc1e (* 𛰞 *) )]

let non_id_hd = [%sedlex.regexp? lambda_lower | lambda_upper | mu_lower]
let id_hd = [%sedlex.regexp? Sub (tr8876_ident_char, non_id_hd) | id_ex]
let id_tl = [%sedlex.regexp? tr8876_ident_char | id_ex | '_' | '0' .. '9']
let id = [%sedlex.regexp? id_hd, Star id_tl | '_', Plus id_tl]

(* *)

let letterlike_symbol = [%sedlex.regexp? 0x2100 .. 0x214f]
let math_alphanumeric_symbol = [%sedlex.regexp? 0x1d400 .. 0x1d7ff]
let math_symbol = [%sedlex.regexp? letterlike_symbol | math_alphanumeric_symbol]
let id_typ_hd = [%sedlex.regexp? id_hd | math_symbol]
let id_typ_tl = [%sedlex.regexp? id_tl | math_symbol]
let id_typ = [%sedlex.regexp? id_typ_hd, Star id_typ_tl | '_', Plus id_typ_tl]

(* *)

let sub_digit = [%sedlex.regexp? 0x2080 .. 0x2089]
let id_sub = [%sedlex.regexp? id_typ, Plus sub_digit]

(* *)

let id_dollar = [%sedlex.regexp? (id_hd | '$'), Star (id_tl | '$')]

(* *)

let line_end = [%sedlex.regexp? '\r' | '\n' | "\r\n" | "\n\r"]

(* *)

let hex_digit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']
let esc_char = [%sedlex.regexp? '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't']
let esc_hex = [%sedlex.regexp? 'u', Rep (hex_digit, 4)]
let char_escaped = [%sedlex.regexp? '\\', (esc_char | esc_hex)]
let char_continued = [%sedlex.regexp? ('\\', white | line_end, Opt white), '\\']
let control_chars = [%sedlex.regexp? 0x0000 .. 0x001f | 0x007f .. 0x009f]
let char_unescaped = [%sedlex.regexp? Compl (control_chars | '"' | '\\')]
let char = [%sedlex.regexp? char_unescaped | char_escaped | char_continued]
let string = [%sedlex.regexp? '"', Star char, '"']

(* *)

let line_directive = [%sedlex.regexp? "#line ", nat_10, ' ', string, line_end]

(* *)

let is_id str =
  let {lexbuf; _} = Buffer.from_utf_8 str in
  match%sedlex lexbuf with id, eof -> true | _ -> false

let is_nat str =
  let {lexbuf; _} = Buffer.from_utf_8 str in
  match%sedlex lexbuf with nat_10, eof -> true | _ -> false

let is_id_or_nat str =
  let {lexbuf; _} = Buffer.from_utf_8 str in
  match%sedlex lexbuf with (id | nat_10), eof -> true | _ -> false

(* *)

let coerce_to_id str =
  let ({lexbuf; _} as buffer) = Buffer.from_utf_8 str in
  let cs = Stack.create () in
  let drop_underscore () = if Stack.top cs = "_" then Stack.pop cs |> ignore in
  let finish () =
    cs |> Stack.to_seq |> List.of_seq |> List.rev |> String.concat ""
  in
  let rec rest () =
    match%sedlex lexbuf with
    | sub_digit -> rest ()
    | "_" | Compl id_tl ->
      non_id @@ fun () ->
      drop_underscore ();
      Stack.push "_" cs;
      if 24 < Stack.length cs then finish () else rest ()
    | id_tl -> and_rest (Buffer.lexeme_utf_8 buffer)
    | _ ->
      drop_underscore ();
      finish ()
  and and_rest c =
    Stack.push c cs;
    rest ()
  and non_id or_else =
    let c = (Buffer.lexeme_utf_8 buffer |> UTF.UTF8.to_uchar_array).(0) in
    match
      id_ex_encodings |> Array.binary_search_opt (fst >>> Uchar.compare c)
    with
    | None -> or_else ()
    | Some (_, s) ->
      Stack.push s cs;
      rest ()
  in
  let rec first () =
    match%sedlex lexbuf with
    | sub_digit -> first ()
    | Compl id_tl -> non_id first
    | id_hd | non_id_hd -> and_rest (Buffer.lexeme_utf_8 buffer)
    | "_" -> and_rest "_"
    | id_tl ->
      Stack.push "_" cs;
      and_rest (Buffer.lexeme_utf_8 buffer)
    | _ -> ""
  in
  first ()

(* *)

let rec token_or_comment ({lexbuf; _} as buffer) =
  let return = return_from buffer in
  let opening tok =
    buffer.state <- `Open :: buffer.state;
    return tok
  and closing tok =
    match buffer.state with
    | `Open :: state | `Initial :: (`TstrStr :: _ as state) ->
      buffer.state <- state;
      return tok
    | _ -> raise @@ Exn_lexeme (Buffer.loc buffer, Buffer.lexeme_utf_8 buffer)
  in
  match buffer.state with
  | ((`Initial | `Open) :: _ | []) as state -> (
    match%sedlex lexbuf with
    | line_directive ->
      Scanf.sscanf (Buffer.lexeme_utf_8 buffer) "#line %d %[^\n\r]"
      @@ fun line filename_literal ->
      let open JsonString in
      let filename = filename_literal |> of_utf8_json |> to_utf8 in
      let _, pos = Sedlexing.lexing_positions lexbuf in
      Sedlexing.set_filename lexbuf filename;
      Sedlexing.set_position lexbuf {pos with pos_lnum = line};
      token_or_comment buffer
    (* *)
    | "%" -> return Percent
    | "'" -> return Tick
    | "(" -> opening ParenLhs
    | ")" -> closing ParenRhs
    | "*" -> return Star
    | "+" -> return Plus
    | "," -> return Comma
    | "-" -> return Minus
    | "." | "=>" -> return Dot
    | "/" -> return Slash
    | ":" -> return Colon
    | "<" -> return Less
    | "=" -> return Equal
    | ">" -> return Greater
    | "[" -> return BracketLhs
    | "]" -> return BracketRhs
    | "^" -> return Caret
    | "{" -> opening BraceLhs
    | "|" -> return Pipe
    | "}" -> closing BraceRhs
    (* *)
    | "_" -> return Underscore
    | "and" -> return And
    | "case" -> return Case
    | "else" -> return Else
    | "exists" | exists -> return Exists
    | "forall" | for_all -> return ForAll
    | "if" -> return If
    | "import" -> return Import
    | "in" -> return In
    | "include" -> return Include
    | "let" -> return Let
    | "local" -> return Local
    | "target" -> return Target
    | "then" -> return Then
    | "type" -> return Type
    (* *)
    | arrow_right | "->" -> return ArrowRight
    | diamond | "<>" -> return Diamond
    | double_angle_quote_lhs | "<<" -> return DoubleAngleQuoteLhs
    | double_angle_quote_rhs | ">>" -> return DoubleAngleQuoteRhs
    | double_comma | ",," -> return DoubleComma
    | greater_equal | ">=" -> return GreaterEqual
    | lambda_lower | "fun" -> return LambdaLower
    | lambda_upper | "gen" -> return LambdaUpper
    | less_equal | "<=" -> return LessEqual
    | logical_and | "&&" -> return LogicalAnd
    | logical_not | "!" -> return LogicalNot
    | logical_or | "||" -> return LogicalOr
    | mu_lower | "rec" -> return MuLower
    | not_equal | "!=" -> return NotEqual
    | triangle_lhs | "<|" -> return TriangleLhs
    | triangle_rhs | "|>" -> return TriangleRhs
    (* *)
    | nat_10 ->
      return
        (LitNat
           (Buffer.lexeme_utf_8 buffer
           |> String.filter (( <> ) '_')
           |> Bigint.of_string))
    (* *)
    | Opt id, '"' ->
      buffer.state <- `TstrStr :: state;
      let tok =
        match Buffer.lexeme_utf_8 buffer with
        | "\"" -> TstrOpenRaw
        | s -> TstrOpen (String.sub s 0 (String.length s - 1))
      and lhs, rhs = Buffer.loc buffer in
      (tok, lhs, Pos.sub_cnum 1 rhs)
    | Opt id, '"', Star char, Opt ('\\', Opt white), eof ->
      buffer.state <- `TstrStr :: state;
      return TstrStrPart
    (* *)
    | id -> return (Id (Buffer.lexeme_utf_8 buffer))
    | id_typ -> return (IdTyp (Buffer.lexeme_utf_8 buffer))
    | id_sub -> return (IdSub (Buffer.lexeme_utf_8 buffer))
    | id_dollar -> return (IdDollar (Buffer.lexeme_utf_8 buffer))
    (* *)
    | comment -> return (Comment (Buffer.lexeme_utf_8 buffer))
    | white -> token_or_comment buffer
    (* *)
    | eof -> return EOF
    (* *)
    | '\\', Plus ('a' .. 'z' | 'A' .. 'Z') ->
      return (Escape (Buffer.lexeme_utf_8 buffer))
    (* *)
    | nat_10, id | any ->
      raise @@ Exn_lexeme (Buffer.loc buffer, Buffer.lexeme_utf_8 buffer)
    | _ -> raise @@ Exn_lexeme (Buffer.loc buffer, Buffer.lexeme_utf_8 buffer))
  | `TstrStr :: state -> (
    match%sedlex lexbuf with
    | Star char ->
      buffer.state <- `TstrEsc :: state;
      return
        (TstrStr
           (Buffer.lexeme_utf_8 buffer |> Printf.sprintf "\"%s\""
          |> JsonString.of_utf8_json_literal))
    | Star char, Opt ('\\', Opt white), eof -> return TstrStrPart
    | _ -> raise @@ Exn_lexeme (Buffer.loc buffer, Buffer.lexeme_utf_8 buffer))
  | `TstrEsc :: state -> (
    match%sedlex lexbuf with
    | '\\', Opt id ->
      buffer.state <- `TstrExp :: state;
      let tok =
        match Buffer.lexeme_utf_8 buffer with
        | "\\" -> TstrEsc "string"
        | s -> TstrEsc (String.sub s 1 (String.length s - 1))
      and lhs, rhs = Buffer.loc buffer in
      (tok, Pos.add_cnum 1 lhs, rhs)
    | '"' ->
      buffer.state <- state;
      return TstrClose
    | _ -> raise @@ Exn_lexeme (Buffer.loc buffer, Buffer.lexeme_utf_8 buffer))
  | `TstrExp :: state -> (
    let return tok =
      buffer.state <- `Initial :: `TstrStr :: state;
      return tok
    in
    match%sedlex lexbuf with
    | '(' -> return ParenLhs
    | '{' -> return BraceLhs
    | _ -> raise @@ Exn_lexeme (Buffer.loc buffer, Buffer.lexeme_utf_8 buffer))

let string_continuation ({lexbuf; _} as buffer) =
  let return = return_from buffer in
  match%sedlex lexbuf with
  | Opt white, '\\' -> return TstrOpenRaw
  | _ -> raise @@ Exn_lexeme (Buffer.loc buffer, Buffer.lexeme_utf_8 buffer)

let rec token buffer =
  match token_or_comment buffer with
  | Comment _, _, _ -> token buffer
  | other -> other

type tok = token * Lexing.position * Lexing.position
type t = Buffer.t -> unit -> tok

let plain : t = fun buffer () -> token buffer

(* *)

module Offside = struct
  open LexTrn

  let error message = loc >>= fun loc -> raise @@ Exn_lexeme (loc, message)

  let expect exp =
    get >>= fun tok ->
    if tok_of tok <> exp then error "unexpected" else emit tok

  (* *)

  let is_closing = function
    | And | BraceRhs | BracketRhs | Comma | DoubleAngleQuoteRhs | EOF | Else
    | In | ParenRhs | Then ->
      true
    | _ -> false

  let rec nest tok =
    (match tok_of tok with
    | ForAll | Exists | MuLower ->
      get >>= fun tok ->
      if tok_of tok <> ParenLhs then emit_before ParenLhs tok else unget tok
    | If | LambdaLower | LambdaUpper | DoubleAngleQuoteLhs ->
      emit (set ParenLhs tok)
    | _ -> unit)
    >> (let ns l tok_ns =
          let* t, _, r = last_tok in
          match t with
          | (Id _ | BraceRhs | BracketRhs | ParenRhs) when l = r ->
            emit (set tok_ns tok)
          | _ -> emit tok
        in
        match tok with
        | BraceLhs, l, _ -> ns l BraceLhsNS
        | BracketLhs, l, _ -> ns l BracketLhsNS
        | ParenLhs, l, _ -> ns l ParenLhsNS
        | _ -> emit tok)
    >> (match tok_of tok with
       | BraceLhs -> with_indent (inside_braces false)
       | ParenLhs -> get >>= nest_until ParenRhs
       | DoubleAngleQuoteLhs ->
         as_typ (get >>= nest_until Comma)
         >> get
         >>= nest_until DoubleAngleQuoteRhs
         >> get
         >>= fun tok ->
         if tok_of tok = Colon then nest tok >>= emit_before ParenRhs
         else emit_before ParenRhs tok
       | BracketLhs -> as_typ (get >>= nest_until BracketRhs)
       | Include -> get >>= insert_in false (col_of tok)
       | Type -> as_typ (binding tok)
       | Let -> binding tok
       | Colon -> as_typ (with_indent (inside_block unget))
       | LambdaLower | LambdaUpper -> get >>= inside_binder
       | ForAll | Exists | MuLower ->
         get >>= fun tok ->
         if tok_of tok <> ParenLhs then inside_binder tok else unget tok
       | If ->
         get >>= nest_until Then >> get >>= nest_until Else
         >> with_indent (inside_block (emit_before ParenRhs))
       | _ -> unit)
    >> get

  and inside_braces insert indent tok =
    match tok_of tok with
    | BraceRhs -> emit tok
    | Comma ->
      let* new_line = new_line tok in
      if new_line && col_of tok < indent - 2 then error "offside"
      else emit tok >> get >>= inside_braces false indent
    | _ ->
      let* new_line = new_line tok in
      if new_line && col_of tok < indent then error "offside"
      else
        emit_if (new_line && col_of tok = indent && insert) (set Comma tok)
        >> nest tok >>= inside_braces true indent

  and insert_in is_rec indent tok =
    match tok_of tok with
    | EOF -> emit tok
    | And ->
      let* new_line = new_line tok in
      if new_line && col_of tok < indent then error "offside"
      else if is_rec then
        emit tok >> expect MuLower >> get >>= insert_in is_rec indent
      else emit tok >> get >>= insert_in is_rec indent
    | In -> if col_of tok < indent then error "offside" else emit tok
    | _ ->
      let* new_line = new_line tok in
      if new_line && col_of tok <= indent then emit_before In tok
      else nest tok >>= insert_in is_rec indent

  and inside_binder tok =
    match tok_of tok with
    | Colon ->
      emit tok
      >> as_typ (with_indent (inside_block unget))
      >> get >>= inside_binder
    | Dot -> emit tok >> with_indent (inside_block (emit_before ParenRhs))
    | _ -> nest tok >>= inside_binder

  and inside_block on_exit indent tok =
    let* is_typ in
    match tok_of tok with
    | (Dot | Equal) when is_typ -> on_exit tok
    | t when is_closing t -> on_exit tok
    | _ ->
      let* new_line = new_line tok in
      if new_line && col_of tok < indent then on_exit tok
      else nest tok >>= inside_block on_exit indent

  and binding tok =
    let indent = col_of tok in
    get >>= fun tok ->
    if tok_of tok = MuLower then emit tok >> get >>= insert_in true indent
    else insert_in false indent tok

  and nest_until closing tok =
    if tok_of tok = closing then emit tok else nest tok >>= nest_until closing
end

let offside : t = LexTrn.init token (LexTrn.get >>= Offside.nest_until EOF)
