open FomBasis
open Lexer
open Grammar

let offset_as_utf_16 input i =
  let input = UTF.UTF8.to_uchar_array input in
  (Array.sub input 0 i |> UTF.UTF16.of_uchar_array |> Bytes.length) / 2

(* *)

module State = struct
  type t = bool

  let initial = false
end

type token_info = {begins : int; ends : int; name : string; state : State.t}

let token_info_utf_8 =
  let atom = "atom"
  and builtin = "type"
  and comment = "comment"
  and error = "error"
  and keyword = "keyword"
  and number = "number"
  and operator = "operator"
  and punctuation = "punctuation"
  and string = "string"
  and tag = "tag"
  and variable = "variable" in
  let to_name = function
    | And -> keyword
    | ArrowRight -> operator
    | BraceLhs -> punctuation
    | BraceRhs -> punctuation
    | BracketLhs -> punctuation
    | BracketLhsNS -> punctuation
    | BracketRhs -> punctuation
    | Caret -> operator
    | Case -> keyword
    | Colon -> punctuation
    | Comma -> punctuation
    | Comment _ -> comment
    | Diamond -> punctuation
    | Dot -> punctuation
    | DoubleAngleQuoteLhs -> punctuation
    | DoubleAngleQuoteRhs -> punctuation
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
    | Local -> keyword
    | LitNat _ -> number
    | LitString _ -> string
    | LitStringPart -> string
    | LogicalAnd -> operator
    | LogicalNot -> operator
    | LogicalOr -> operator
    | Minus -> operator
    | MuLower -> tag
    | NotEqual -> operator
    | ParenLhs -> punctuation
    | ParenLhsNS -> punctuation
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
    | Underscore -> variable
  in
  fun state ->
    let lexer = if state then string_continuation else token_or_comment in
    Buffer.from_utf_8 >>> lexer >>> fun (token, lhs, rhs) ->
    {
      begins = lhs.pos_cnum;
      ends = rhs.pos_cnum;
      name = to_name token;
      state = token = LitStringPart;
    }
