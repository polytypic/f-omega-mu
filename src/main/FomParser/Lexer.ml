open FomBasis
open FomSource
open Grammar

exception Exn_lexeme of Loc.t * string

let return_from buffer tok =
  let lhs, rhs = Buffer.loc buffer in
  (tok, lhs, rhs)

let double_angle_lhs = [%sedlex.regexp? 0x300a (* 《 *)]
let double_angle_rhs = [%sedlex.regexp? 0x300b (* 》 *)]
let lambda_lower = [%sedlex.regexp? 0x03bb (* λ *)]
let lambda_upper = [%sedlex.regexp? 0x039b (* Λ *)]
let arrow_right = [%sedlex.regexp? 0x2192 (* → *)]
let mu_lower = [%sedlex.regexp? 0x03bc (* μ *)]
let for_all = [%sedlex.regexp? 0x2200 (* ∀ *)]
let exists = [%sedlex.regexp? 0x2203 (* ∃ *)]
let less_equal = [%sedlex.regexp? 0x2264 (* ≤ *)]
let greater_equal = [%sedlex.regexp? 0x2265 (* ≥ *)]
let logical_and = [%sedlex.regexp? 0x2227 (* ∧ *)]
let logical_or = [%sedlex.regexp? 0x2228 (* ∨ *)]
let logical_not = [%sedlex.regexp? 0x00ac (* ¬ *)]
let not_equal = [%sedlex.regexp? 0x2260 (* ≠ *)]
let triangle_lhs = [%sedlex.regexp? 0x25c1 (* ◁ *)]
let triangle_rhs = [%sedlex.regexp? 0x25b7 (* ▷ *)]
let diamond = [%sedlex.regexp? 0x25c7 (* ◇ *)]

(* *)

let comment = [%sedlex.regexp? "#", Star (Compl ('\n' | '\r'))]
let whitespace = [%sedlex.regexp? Plus (Chars " \t\n\r")]
let nat_10 = [%sedlex.regexp? "0" | '1' .. '9', Star (Opt '_', Plus '0' .. '9')]

(* *)

let sub_digit = [%sedlex.regexp? 0x2080 .. 0x2089]

(* *)

let id_first =
  [%sedlex.regexp?
    Sub (tr8876_ident_char, (lambda_lower | lambda_upper | mu_lower))]

let id_rest = [%sedlex.regexp? tr8876_ident_char | '_' | '0' .. '9']
let id = [%sedlex.regexp? id_first, Star id_rest | '_', Plus id_rest]

let math_symbol =
  [%sedlex.regexp?
    (* Letterlike Symbols *)
    ( 0x2100 .. 0x210f
    | 0x2110 .. 0x211f
    | 0x2120 .. 0x212f
    | 0x2130 .. 0x213f
    | 0x2140 .. 0x214f
    (* Mathematical Alphanumeric Symbols *)
    | 0x1d400 .. 0x1d40f
    | 0x1d410 .. 0x1d41f
    | 0x1d420 .. 0x1d42f
    | 0x1d430 .. 0x1d43f
    | 0x1d440 .. 0x1d44f
    | 0x1d450 .. 0x1d45f
    | 0x1d460 .. 0x1d46f
    | 0x1d470 .. 0x1d47f
    | 0x1d480 .. 0x1d48f
    | 0x1d490 .. 0x1d49f
    | 0x1d4a0 .. 0x1d4af
    | 0x1d4b0 .. 0x1d4bf
    | 0x1d4c0 .. 0x1d4cf
    | 0x1d4d0 .. 0x1d4df
    | 0x1d4e0 .. 0x1d4ef
    | 0x1d4f0 .. 0x1d4ff
    | 0x1d500 .. 0x1d50f
    | 0x1d510 .. 0x1d51f
    | 0x1d520 .. 0x1d52f
    | 0x1d530 .. 0x1d53f
    | 0x1d540 .. 0x1d54f
    | 0x1d550 .. 0x1d55f
    | 0x1d560 .. 0x1d56f
    | 0x1d570 .. 0x1d57f
    | 0x1d580 .. 0x1d58f
    | 0x1d590 .. 0x1d59f
    | 0x1d5a0 .. 0x1d5af
    | 0x1d5b0 .. 0x1d5bf
    | 0x1d5c0 .. 0x1d5cf
    | 0x1d5d0 .. 0x1d5df
    | 0x1d5e0 .. 0x1d5ef
    | 0x1d5f0 .. 0x1d5ff
    | 0x1d600 .. 0x1d60f
    | 0x1d610 .. 0x1d61f
    | 0x1d620 .. 0x1d62f
    | 0x1d630 .. 0x1d63f
    | 0x1d640 .. 0x1d64f
    | 0x1d650 .. 0x1d65f
    | 0x1d660 .. 0x1d66f
    | 0x1d670 .. 0x1d67f
    | 0x1d680 .. 0x1d68f
    | 0x1d690 .. 0x1d69f
    | 0x1d6a0 .. 0x1d6af
    | 0x1d6b0 .. 0x1d6bf
    | 0x1d6c0 .. 0x1d6cf
    | 0x1d6d0 .. 0x1d6df
    | 0x1d6e0 .. 0x1d6ef
    | 0x1d6f0 .. 0x1d6ff
    | 0x1d700 .. 0x1d70f
    | 0x1d710 .. 0x1d71f
    | 0x1d720 .. 0x1d72f
    | 0x1d730 .. 0x1d73f
    | 0x1d740 .. 0x1d74f
    | 0x1d750 .. 0x1d75f
    | 0x1d760 .. 0x1d76f
    | 0x1d770 .. 0x1d77f
    | 0x1d780 .. 0x1d78f
    | 0x1d790 .. 0x1d79f
    | 0x1d7a0 .. 0x1d7af
    | 0x1d7b0 .. 0x1d7bf
    | 0x1d7c0 .. 0x1d7cf
    | 0x1d7d0 .. 0x1d7df
    | 0x1d7e0 .. 0x1d7ef
    | 0x1d7f0 .. 0x1d7ff )]

let id_typ =
  [%sedlex.regexp?
    ( (id_first | math_symbol), Star (id_rest | math_symbol)
    | '_', Plus (id_rest | math_symbol) )]

let id_sub = [%sedlex.regexp? id_typ, Plus sub_digit]

(* *)

let hex_digit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']

let char_escaped =
  [%sedlex.regexp?
    ( '\\',
      ('"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' | 'u', Rep (hex_digit, 4))
    )]

let char_unescaped =
  [%sedlex.regexp? Compl (0x0000 .. 0x001f | 0x007f .. 0x009f | '"' | '\\')]

let char = [%sedlex.regexp? char_unescaped | char_escaped]
let string = [%sedlex.regexp? "\"", Star char, "\""]

(* *)

let rec token_or_comment buffer =
  let return = return_from buffer in
  match%sedlex buffer with
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
  | "\\" -> return Backslash
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
  | "target" -> return Target
  | "then" -> return Then
  | "type" -> return Type
  (* *)
  | arrow_right | "->" -> return ArrowRight
  | double_angle_lhs | "<<" -> return DoubleAngleLhs
  | double_angle_rhs | ">>" -> return DoubleAngleRhs
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
  | diamond | "<>" -> return Diamond
  (* *)
  | nat_10 ->
    return
      (LitNat
         (Buffer.lexeme_utf_8 buffer |> String.to_seq
         |> Seq.filter (( <> ) '_')
         |> String.of_seq |> Bigint.of_string))
  (* *)
  | string ->
    return
      (LitString (Buffer.lexeme_utf_8 buffer |> FomCST.LitString.of_utf8_json))
  (* *)
  | id -> return (Id (Buffer.lexeme_utf_8 buffer))
  | id_typ -> return (IdTyp (Buffer.lexeme_utf_8 buffer))
  | id_sub -> return (IdSub (Buffer.lexeme_utf_8 buffer))
  (* *)
  | comment -> return (Comment (Buffer.lexeme_utf_8 buffer))
  | whitespace -> token_or_comment buffer
  (* *)
  | eof -> return EOF
  (* *)
  | nat_10, id | any ->
    raise @@ Exn_lexeme (Buffer.loc buffer, Buffer.lexeme_utf_8 buffer)
  | _ -> raise @@ Exn_lexeme (Buffer.loc buffer, Buffer.lexeme_utf_8 buffer)

let rec token buffer =
  match token_or_comment buffer with
  | Comment _, _, _ -> token buffer
  | other -> other

type tok = token * Lexing.position * Lexing.position
type t = Buffer.t -> unit -> tok

let plain : t = fun buffer () -> token buffer

type token_info = {begins : int; ends : int; name : string}

let atom = "atom"
let builtin = "type"
let comment = "comment"
let error = "error"
let keyword = "keyword"
let number = "number"
let operator = "operator"
let punctuation = "punctuation"
let string = "string"
let tag = "tag"
let variable = "variable"

let offset_as_utf_16 input i =
  let input = UTF8.to_uchar_array input in
  (Array.sub input 0 i |> UTF16.of_uchar_array |> Bytes.length) / 2

let token_info_utf_8 input =
  Buffer.from_utf_8 input |> token_or_comment |> fun (token, lhs, rhs) ->
  {
    begins = lhs.pos_cnum;
    ends = rhs.pos_cnum;
    name =
      (match token with
      | And -> keyword
      | ArrowRight -> operator
      | Backslash -> punctuation
      | BraceLhs -> punctuation
      | BraceRhs -> punctuation
      | BracketLhs -> punctuation
      | BracketRhs -> punctuation
      | Caret -> operator
      | Case -> keyword
      | Colon -> punctuation
      | Comma -> punctuation
      | Comment _ -> comment
      | Diamond -> punctuation
      | Dot -> punctuation
      | DoubleAngleLhs -> punctuation
      | DoubleAngleRhs -> punctuation
      | EOF -> error
      | Else -> keyword
      | Equal -> operator
      | Exists -> tag
      | ForAll -> tag
      | Greater -> operator
      | GreaterEqual -> operator
      | Id ("bool" | "int" | "impure" | "string") -> builtin
      | Id ("true" | "false") -> atom
      | Id _ -> variable
      | IdTyp _ -> variable
      | IdSub _ -> variable
      | If -> keyword
      | Import -> keyword
      | In -> keyword
      | Include -> keyword
      | LambdaLower -> tag
      | LambdaUpper -> tag
      | Less -> operator
      | LessEqual -> operator
      | Let -> keyword
      | LitNat _ -> number
      | LitString _ -> string
      | LogicalAnd -> operator
      | LogicalNot -> operator
      | LogicalOr -> operator
      | Minus -> operator
      | MuLower -> tag
      | NotEqual -> operator
      | ParenLhs -> punctuation
      | ParenRhs -> punctuation
      | Percent -> operator
      | Pipe -> punctuation
      | Plus -> operator
      | Slash -> operator
      | Star -> operator
      | Target -> keyword
      | Then -> keyword
      | Tick -> punctuation
      | TriangleLhs -> punctuation
      | TriangleRhs -> punctuation
      | Type -> keyword
      | Underscore -> variable);
  }

let[@warning "-32"] to_string = function
  | And -> "and"
  | ArrowRight -> "→"
  | Backslash -> "\\"
  | BraceLhs -> "{"
  | BraceRhs -> "}"
  | BracketLhs -> "["
  | BracketRhs -> "]"
  | Caret -> "^"
  | Case -> "case"
  | Colon -> ":"
  | Comma -> ","
  | Comment _ -> "# ..."
  | Diamond -> "◇"
  | Dot -> "."
  | DoubleAngleLhs -> "《"
  | DoubleAngleRhs -> "》"
  | EOF -> "<EOF>"
  | Else -> "else"
  | Equal -> "="
  | Exists -> "∃"
  | ForAll -> "∀"
  | Greater -> ">"
  | GreaterEqual -> "≥"
  | Id id -> id
  | IdTyp id -> id
  | IdSub id -> id
  | If -> "if"
  | Import -> "import"
  | In -> "in"
  | Include -> "include"
  | LambdaLower -> "λ"
  | LambdaUpper -> "Λ"
  | Less -> "<"
  | LessEqual -> "≤"
  | Let -> "let"
  | LitNat n -> Bigint.to_string n
  | LitString s -> FomAST.LitString.to_utf8_json s
  | LogicalAnd -> "∧"
  | LogicalNot -> "∨"
  | LogicalOr -> "¬"
  | Minus -> "-"
  | MuLower -> "μ"
  | NotEqual -> "≠"
  | ParenLhs -> "("
  | ParenRhs -> ")"
  | Percent -> "%"
  | Pipe -> "|"
  | Plus -> "+"
  | Slash -> "/"
  | Star -> "*"
  | Target -> "target"
  | Then -> "then"
  | Tick -> "'"
  | TriangleLhs -> "◁"
  | TriangleRhs -> "▷"
  | Type -> "type"
  | Underscore -> "_"

module Offside = struct
  type 'a monad =
    | Monad of
        ((Buffer.t -> tok) ->
        Buffer.t ->
        Lexing.position ->
        bool ->
        tok option ->
        'a result * bool * tok option)

  and 'a result = Emit of tok * 'a monad | Return of 'a

  let return value =
    Monad (fun _ _ _ is_typ tok_opt -> (Return value, is_typ, tok_opt))

  let unit = return ()

  let rec ( >>= ) (Monad xM) xyM =
    Monad
      (fun get_tok buffer last_pos is_typ tok_opt ->
        match xM get_tok buffer last_pos is_typ tok_opt with
        | Emit (tok, xM), is_typ, tok_opt ->
          (Emit (tok, xM >>= xyM), is_typ, tok_opt)
        | Return x, is_typ, tok_opt ->
          let (Monad yM) = xyM x in
          yM get_tok buffer last_pos is_typ tok_opt)

  let ( let* ) = ( >>= )
  let ( >> ) lhs rhs = lhs >>= fun () -> rhs

  (* *)

  let col_of (_, (p : Lexing.position), _) = p.pos_cnum - p.pos_bol
  let tok_of (t, _, _) = t

  let set token (_, s, (e : Lexing.position)) =
    (token, s, {e with pos_bol = e.pos_bol - 1})

  (* *)

  let get =
    Monad
      (fun get_tok buffer _ is_typ tok_opt ->
        ( Return (match tok_opt with Some tok -> tok | None -> get_tok buffer),
          is_typ,
          None ))

  let unget tok =
    Monad
      (fun _ _ _ is_typ tok_opt ->
        match tok_opt with
        | Some _ -> failwith "unget"
        | None -> (Return (), is_typ, Some tok))

  let error message =
    Monad (fun _ buffer _ -> raise @@ Exn_lexeme (Buffer.loc buffer, message))

  let emit tok =
    Monad (fun _ _ _ is_typ tok_opt -> (Emit (tok, unit), is_typ, tok_opt))

  let emit_if bool tok = if bool then emit tok else unit
  let emit_before tok token = unget tok >> emit (set token tok)

  let expect exp =
    get >>= fun tok ->
    if tok_of tok <> exp then error "unexpected" else emit tok

  let new_line (_, (p : Lexing.position), _) =
    Monad
      (fun _ _ last_pos is_typ tok_opt ->
        (Return (last_pos.pos_bol <> p.pos_bol), is_typ, tok_opt))

  let is_typ =
    Monad (fun _ _ _ is_typ tok_opt -> (Return is_typ, is_typ, tok_opt))

  let set_is_typ is_typ =
    Monad (fun _ _ _ _ tok_opt -> (Return (), is_typ, tok_opt))

  let as_typ op =
    let* was = is_typ in
    set_is_typ true >> op >>= fun res -> set_is_typ was >> return res

  let with_indent rule = get >>= fun tok -> rule (col_of tok) tok

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

  and insert_in indent tok =
    match tok_of tok with
    | EOF -> emit tok
    | And ->
      let* new_line = new_line tok in
      if new_line && col_of tok < indent then
        error "offside"
      else
        emit tok >> expect MuLower >> get >>= insert_in indent
    | In -> if col_of tok < indent then error "offside" else emit tok
    | _ ->
      let* new_line = new_line tok in
      if new_line && col_of tok < indent then
        error "offside"
      else if new_line && col_of tok = indent then
        emit_before tok In
      else
        nest tok >>= insert_in indent

  and inside_binder tok =
    match tok_of tok with
    | Colon ->
      emit tok >> as_typ (with_indent inside_annot) >> get >>= inside_binder
    | Dot -> emit tok >> with_indent inside_body
    | _ -> nest tok >>= inside_binder

  and inside_annot indent tok =
    match tok_of tok with
    | BraceRhs | BracketRhs | Comma | Dot | DoubleAngleRhs | EOF | Equal | In
    | ParenRhs ->
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
    | BraceRhs | BracketRhs | Comma | DoubleAngleRhs | EOF | Else | In
    | ParenRhs ->
      emit_before tok ParenRhs
    | (Dot | Equal | Backslash) when is_typ -> emit_before tok ParenRhs
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
    | BraceRhs | BracketRhs | Comma | DoubleAngleRhs | EOF | ParenRhs ->
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
    | If | LambdaLower | LambdaUpper | DoubleAngleLhs -> emit (set ParenLhs tok)
    | _ -> unit)
    >> emit tok
    >> (match tok_of tok with
       | BraceLhs -> with_indent (inside_braces false)
       | ParenLhs -> get >>= nest_until ParenRhs
       | DoubleAngleLhs ->
         as_typ (get >>= nest_until Backslash)
         >> get >>= nest_until DoubleAngleRhs >> get
         >>= fun tok ->
         if tok_of tok = Colon then
           nest tok >>= fun tok -> emit_before tok ParenRhs
         else
           emit_before tok ParenRhs
       | BracketLhs -> as_typ (get >>= nest_until BracketRhs)
       | Include -> get >>= insert_in (col_of tok)
       | Type ->
         let indent = col_of tok in
         get >>= fun tok ->
         if tok_of tok = MuLower then
           as_typ (emit tok >> get >>= insert_in indent)
         else
           as_typ (insert_in indent tok)
       | Let ->
         let indent = col_of tok in
         get >>= fun tok ->
         if tok_of tok = MuLower then
           emit tok >> get >>= insert_in indent
         else
           insert_in indent tok
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

  (* *)

  type state = (unit monad * Lexing.position * bool * tok option) ref

  let init () : state = ref (get >>= initial, Lexing.dummy_pos, false, None)
end

let offside : t =
 fun buffer ->
  let state = Offside.init () in
  fun () ->
    let Monad uM, last_pos, is_typ, tok_opt = !state in
    match uM token buffer last_pos is_typ tok_opt with
    | Emit (((_, _, last_pos) as tok), continue), is_typ, tok_opt ->
      state := (continue, last_pos, is_typ, tok_opt);
      tok
    | Return (), _, _ -> failwith "return"
