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
let id_sub = [%sedlex.regexp? id, Plus sub_digit]

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

let lit_true = LitBool true
let lit_false = LitBool false

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
  | "{" -> return BraceLhs
  | "|" -> return Pipe
  | "}" -> return BraceRhs
  (* *)
  | "_" -> return Underscore
  | "and" -> return And
  | "case" -> return Case
  | "else" -> return Else
  | "exists" | exists -> return Exists
  | "false" -> return lit_false
  | "forall" | for_all -> return ForAll
  | "if" -> return If
  | "import" -> return Import
  | "in" -> return In
  | "include" -> return Include
  | "let" -> return Let
  | "target" -> return Target
  | "then" -> return Then
  | "true" -> return lit_true
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

let token_info_utf_8 input =
  Buffer.from_utf_8 input |> token_or_comment |> fun (token, lhs, rhs) ->
  {
    begins = lhs.pos_cnum - lhs.pos_bol;
    ends = rhs.pos_cnum - lhs.pos_bol;
    name =
      (match token with
      | And -> keyword
      | ArrowRight -> operator
      | Backslash -> punctuation
      | BraceLhs -> punctuation
      | BraceRhs -> punctuation
      | BracketLhs -> punctuation
      | BracketRhs -> punctuation
      | Case -> keyword
      | Colon -> punctuation
      | Comma -> punctuation
      | Comment _ -> comment
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
      | Id ("bool" | "int" | "string") -> builtin
      | Id _ -> variable
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
      | LitBool _ -> atom
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

module Offside = struct
  type 'a monad =
    | Monad of
        ((Buffer.t -> tok) -> Buffer.t -> tok option -> 'a result * tok option)

  and 'a result = Emit of tok * 'a monad | Return of 'a

  let return value = Monad (fun _ _ tok_opt -> (Return value, tok_opt))
  let unit = return ()

  let rec ( >>= ) (Monad xM) xyM =
    Monad
      (fun get_tok buffer tok_opt ->
        match xM get_tok buffer tok_opt with
        | Emit (tok, xM), tok_opt -> (Emit (tok, xM >>= xyM), tok_opt)
        | Return x, tok_opt ->
          let (Monad yM) = xyM x in
          yM get_tok buffer tok_opt)

  let ( let* ) = ( >>= )
  let ( >> ) lhs rhs = lhs >>= fun () -> rhs

  (* *)

  let get =
    Monad
      (fun get_tok buffer tok_opt ->
        ( Return (match tok_opt with Some tok -> tok | None -> get_tok buffer),
          None ))

  let unget tok =
    Monad
      (fun get_tok buffer tok_opt ->
        match tok_opt with
        | Some _ -> failwith "unget"
        | None -> (Return (), Some tok))

  let error message =
    Monad (fun _ buffer _ -> raise @@ Exn_lexeme (Buffer.loc buffer, message))

  let emit tok = Monad (fun _ _ tok_opt -> (Emit (tok, unit), tok_opt))
  let emit_if bool tok = if bool then emit tok else unit

  (* *)

  let col_of (_, (p : Lexing.position), _) = p.pos_cnum - p.pos_bol
  let tok_of (t, _, _) = t
  let set token (_, s, e) = (token, s, e)

  (* *)

  let rec initial tok = match tok_of tok with _ -> nest tok >> get >>= initial

  and inside_braces insert indent tok =
    match tok_of tok with
    | BraceRhs -> emit tok
    | Comma ->
      if col_of tok < indent - 2 then
        error "offside"
      else
        emit tok >> get >>= inside_braces false indent
    | _ ->
      if col_of tok < indent then
        error "offside"
      else
        emit_if (col_of tok = indent && insert) (set Comma tok)
        >> nest tok >> get >>= inside_braces true indent

  and inside_parens closing tok =
    if tok_of tok = closing then
      emit tok
    else
      nest tok >> get >>= inside_parens closing

  and insert_in indent tok =
    match tok_of tok with
    | EOF -> emit tok
    | And ->
      if col_of tok < indent then
        error "offside"
      else
        emit tok >> get >>= insert_in indent
    | In -> if col_of tok < indent then error "offside" else emit tok
    | _ ->
      if col_of tok < indent then
        error "offside"
      else if col_of tok = indent then
        unget tok >> emit (set In tok)
      else
        nest tok >> get >>= insert_in indent

  and nest tok =
    emit tok
    >>
    match tok_of tok with
    | BraceLhs -> get >>= fun tok -> inside_braces false (col_of tok) tok
    | ParenLhs -> get >>= inside_parens ParenRhs
    | DoubleAngleLhs -> get >>= inside_parens DoubleAngleRhs
    | BracketLhs -> get >>= inside_parens BracketRhs
    | Type | Include | Let -> get >>= insert_in (col_of tok)
    | _ -> unit

  (* *)

  type state = (unit monad * tok option) ref

  let init () : state = ref (get >>= initial, None)
end

let offside : t =
 fun buffer ->
  let state = Offside.init () in
  fun () ->
    let Monad uM, tok_opt = !state in
    match uM token buffer tok_opt with
    | Emit (tok, continue), tok_opt ->
      state := (continue, tok_opt);
      tok
    | Return (), _ -> failwith "return"
