open FomDiag
open Grammar

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
let comment = [%sedlex.regexp? "#", Star (Compl ('\n' | '\r'))]
let whitespace = [%sedlex.regexp? Plus (Chars " \t\n\r")]
let nat_10 = [%sedlex.regexp? "0" | '1' .. '9', Star '0' .. '9']

(* *)

let id_first =
  [%sedlex.regexp?
    Sub (tr8876_ident_char, (lambda_lower | lambda_upper | mu_lower))]

let id_rest = [%sedlex.regexp? tr8876_ident_char | '_' | '0' .. '9']
let id = [%sedlex.regexp? id_first, Star id_rest | '_', Plus id_rest]

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
  | "{" -> return BraceLhs
  | "}" -> return BraceRhs
  (* *)
  | "_" -> return Underscore
  | "and" -> return And
  | "bool" -> return Bool
  | "case" -> return Case
  | "else" -> return Else
  | "exists" | exists -> return Exists
  | "false" -> return lit_false
  | "forall" | for_all -> return ForAll
  | "if" -> return If
  | "in" -> return In
  | "int" -> return Int
  | "let" -> return Let
  | "string" -> return String
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
  (* *)
  | nat_10 -> return (LitNat (Buffer.lexeme_utf_8 buffer |> Bigint.of_string))
  (* *)
  | string -> return (LitString (Buffer.lexeme_utf_8 buffer))
  (* *)
  | id -> return (Id (Buffer.lexeme_utf_8 buffer))
  (* *)
  | comment -> return (Comment (Buffer.lexeme_utf_8 buffer))
  | whitespace -> token_or_comment buffer
  (* *)
  | eof -> return EOF
  (* *)
  | nat_10, id | any ->
    Error.syntax (Buffer.loc buffer) (Buffer.lexeme_utf_8 buffer)
  | _ -> Error.syntax (Buffer.loc buffer) (Buffer.lexeme_utf_8 buffer)

let rec token buffer =
  match token_or_comment buffer with
  | Comment _, _, _ -> token buffer
  | other -> other

type t = Buffer.t -> unit -> token * Lexing.position * Lexing.position

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
      | Bool -> builtin
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
      | Id _ -> variable
      | If -> keyword
      | In -> keyword
      | Int -> builtin
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
      | Plus -> operator
      | Slash -> operator
      | Star -> operator
      | String -> builtin
      | Target -> keyword
      | Then -> keyword
      | Type -> keyword
      | Underscore -> variable);
  }
