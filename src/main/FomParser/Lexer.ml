open FomBasis
open FomSource
open Grammar

exception Exn_lexeme of Loc.t * string

let return_from buffer tok =
  let lhs, rhs = Buffer.loc buffer in
  (tok, lhs, rhs)

let arrow_right = [%sedlex.regexp? 0x2192 (* → *)]
let diamond = [%sedlex.regexp? 0x25c7 (* ◇ *)]
let double_angle_quote_lhs = [%sedlex.regexp? 0x00ab (* « *)]
let double_angle_quote_rhs = [%sedlex.regexp? 0x00bb (* » *)]
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
let whitespace = [%sedlex.regexp? Plus (Chars " \t\n\r")]
let nat_10 = [%sedlex.regexp? "0" | '1' .. '9', Star (Opt '_', Plus '0' .. '9')]

(* *)

let non_id_hd = [%sedlex.regexp? lambda_lower | lambda_upper | mu_lower]
let id_hd = [%sedlex.regexp? Sub (tr8876_ident_char, non_id_hd)]
let id_tl = [%sedlex.regexp? tr8876_ident_char | '_' | '0' .. '9']
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

let hex_digit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']
let esc_char = [%sedlex.regexp? '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't']
let esc_hex = [%sedlex.regexp? 'u', Rep (hex_digit, 4)]
let char_escaped = [%sedlex.regexp? '\\', (esc_char | esc_hex)]
let char_continued = [%sedlex.regexp? '\\', whitespace, '\\']
let control_chars = [%sedlex.regexp? 0x0000 .. 0x001f | 0x007f .. 0x009f]
let char_unescaped = [%sedlex.regexp? Compl (control_chars | '"' | '\\')]
let char = [%sedlex.regexp? char_unescaped | char_escaped | char_continued]
let string = [%sedlex.regexp? '"', Star char, '"']

(* *)

let line_end = [%sedlex.regexp? '\r' | '\n' | "\r\n" | "\n\r"]
let line_directive = [%sedlex.regexp? "#line ", nat_10, ' ', string, line_end]

(* *)

let rec token_or_comment buffer =
  let return = return_from buffer in
  match%sedlex buffer with
  | line_directive ->
    Scanf.sscanf (Buffer.lexeme_utf_8 buffer) "#line %d %[^\n\r]"
    @@ fun line filename_literal ->
    let open JsonString in
    let filename = filename_literal |> of_utf8_json |> to_utf8 in
    let _, pos = Sedlexing.lexing_positions buffer in
    Sedlexing.set_filename buffer filename;
    Sedlexing.set_position buffer {pos with pos_lnum = line};
    token_or_comment buffer
  (* *)
  | "%" -> return Percent
  | "'" -> return Tick
  | "(" -> return ParenLhs
  | ")" -> return ParenRhs
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
  | "{" -> return BraceLhs
  | "|" -> return Pipe
  | "}" -> return BraceRhs
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
  | string ->
    return
      (LitString (Buffer.lexeme_utf_8 buffer |> JsonString.of_utf8_json_literal))
  | '"', Star char, '\\', Opt whitespace, eof -> return LitStringPart
  (* *)
  | id -> return (Id (Buffer.lexeme_utf_8 buffer))
  | id_typ -> return (IdTyp (Buffer.lexeme_utf_8 buffer))
  | id_sub -> return (IdSub (Buffer.lexeme_utf_8 buffer))
  | id_dollar -> return (IdDollar (Buffer.lexeme_utf_8 buffer))
  (* *)
  | comment -> return (Comment (Buffer.lexeme_utf_8 buffer))
  | whitespace -> token_or_comment buffer
  (* *)
  | eof -> return EOF
  (* *)
  | nat_10, id | '"', Star char | any ->
    raise @@ Exn_lexeme (Buffer.loc buffer, Buffer.lexeme_utf_8 buffer)
  | _ -> raise @@ Exn_lexeme (Buffer.loc buffer, Buffer.lexeme_utf_8 buffer)

let string_continuation buffer =
  let return = return_from buffer in
  match%sedlex buffer with
  | Opt whitespace, '\\', Star char, '\\', Opt whitespace, eof ->
    return LitStringPart
  | Opt whitespace, '\\', Star char, '"', Opt whitespace, eof ->
    return (LitString (JsonString.of_utf8_json "\"\""))
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

  let rec initial tok = match tok_of tok with _ -> nest tok >>= initial

  and inside_braces insert indent tok =
    match tok_of tok with
    | BraceRhs -> emit tok
    | Comma ->
      let* new_line = new_line tok in
      if new_line && col_of tok < indent - 2 then
        error "offside"
      else
        emit tok >> get >>= inside_braces false indent
    | _ ->
      let* new_line = new_line tok in
      if new_line && col_of tok < indent then
        error "offside"
      else
        emit_if (new_line && col_of tok = indent && insert) (set Comma tok)
        >> nest tok >>= inside_braces true indent

  and insert_in is_rec indent tok =
    match tok_of tok with
    | EOF -> emit tok
    | And ->
      let* new_line = new_line tok in
      if new_line && col_of tok < indent then
        error "offside"
      else if is_rec then
        emit tok >> expect MuLower >> get >>= insert_in is_rec indent
      else
        emit tok >> get >>= insert_in is_rec indent
    | In -> if col_of tok < indent then error "offside" else emit tok
    | _ ->
      let* new_line = new_line tok in
      if new_line && col_of tok <= indent then
        emit_before tok In
      else
        nest tok >>= insert_in is_rec indent

  and inside_binder tok =
    match tok_of tok with
    | Colon ->
      emit tok >> as_typ (with_indent inside_annot) >> get >>= inside_binder
    | Dot -> emit tok >> with_indent inside_body
    | _ -> nest tok >>= inside_binder

  and inside_annot indent tok =
    match tok_of tok with
    | BraceRhs | BracketRhs | Comma | Dot | DoubleAngleQuoteRhs | EOF | Equal
    | In | ParenRhs ->
      unget tok
    | _ ->
      let* new_line = new_line tok in
      if new_line && col_of tok < indent then
        unget tok
      else
        nest tok >>= inside_annot indent

  and inside_body indent tok =
    let* is_typ = is_typ in
    match tok_of tok with
    | And | BraceRhs | BracketRhs | Comma | DoubleAngleQuoteRhs | EOF | Else
    | In | ParenRhs ->
      emit_before tok ParenRhs
    | (Dot | Equal) when is_typ -> emit_before tok ParenRhs
    | _ ->
      let* new_line = new_line tok in
      if new_line && col_of tok < indent then
        emit_before tok ParenRhs
      else
        nest tok >>= inside_body indent

  and inside_if indent tok =
    match tok_of tok with
    | Then -> emit tok >> with_indent inside_then
    | _ -> nest tok >>= inside_if indent

  and inside_then indent tok =
    match tok_of tok with
    | Else -> emit tok >> with_indent inside_else
    | _ -> nest tok >>= inside_then indent

  and inside_else indent tok =
    match tok_of tok with
    | BraceRhs | BracketRhs | Comma | DoubleAngleQuoteRhs | EOF | ParenRhs ->
      emit_before tok ParenRhs
    | _ ->
      let* new_line = new_line tok in
      if new_line && col_of tok < indent then
        emit_before tok ParenRhs
      else
        nest tok >>= inside_else indent

  and nest_until closing tok =
    if tok_of tok = closing then
      emit tok
    else
      nest tok >>= nest_until closing

  and nest tok =
    (match tok_of tok with
    | ForAll | Exists | MuLower ->
      get >>= fun tok ->
      if tok_of tok <> ParenLhs then emit_before tok ParenLhs else unget tok
    | If | LambdaLower | LambdaUpper | DoubleAngleQuoteLhs ->
      emit (set ParenLhs tok)
    | _ -> unit)
    >> (match tok with
       | BracketLhs, l, _ -> (
         let* t, _, r = last_tok in
         match t with
         | (Id _ | BracketRhs | ParenRhs) when l = r ->
           emit (set BracketLhsNS tok)
         | _ -> emit tok)
       | ParenLhs, l, _ -> (
         let* t, _, r = last_tok in
         match t with
         | (Id _ | BracketRhs | ParenRhs) when l = r ->
           emit (set ParenLhsNS tok)
         | _ -> emit tok)
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
         if tok_of tok = Colon then
           nest tok >>= fun tok -> emit_before tok ParenRhs
         else
           emit_before tok ParenRhs
       | BracketLhs -> as_typ (get >>= nest_until BracketRhs)
       | Include -> get >>= insert_in false (col_of tok)
       | Type ->
         let indent = col_of tok in
         get >>= fun tok ->
         if tok_of tok = MuLower then
           as_typ (emit tok >> get >>= insert_in true indent)
         else
           as_typ (insert_in false indent tok)
       | Let ->
         let indent = col_of tok in
         get >>= fun tok ->
         if tok_of tok = MuLower then
           emit tok >> get >>= insert_in true indent
         else
           insert_in false indent tok
       | Colon -> as_typ (with_indent inside_annot)
       | LambdaLower | LambdaUpper -> get >>= inside_binder
       | ForAll | Exists | MuLower ->
         get >>= fun tok ->
         if tok_of tok <> ParenLhs then
           inside_binder tok
         else
           unget tok
       | If -> with_indent inside_if
       | _ -> unit)
    >> get
end

let offside : t = LexTrn.init token (LexTrn.get >>= Offside.initial)
