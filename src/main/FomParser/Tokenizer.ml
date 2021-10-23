open FomBasis
open Lexer
open Grammar

let offset_as_utf_16 input i =
  let input = UTF.UTF8.to_uchar_array input in
  let n = Array.length input in
  (Array.sub input 0 (min i n) |> UTF.UTF16.of_uchar_array |> Bytes.length) / 2

let offset_as_utf_32 input i =
  let input = UTF.UTF8.to_uchar_array input |> UTF.UTF16.of_uchar_array in
  let input = Bytes.sub input 0 (min (i * 2) (Bytes.length input)) in
  let uchars = UTF.UTF16.to_uchar_array input in
  Array.length uchars

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
    | DoubleComma -> operator
    | EOF -> error
    | Else -> keyword
    | Equal -> operator
    | Escape _ -> string
    | Exists -> tag
    | ForAll -> tag
    | Greater -> operator
    | GreaterEqual -> operator
    | Id ("bool" | "int" | "impure" | "string") -> builtin
    | Id ("true" | "false") -> atom
    | Id _ -> variable
    | IdDollar _ -> variable
    | IdSub _ -> variable
    | IdTyp _ -> variable
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
    | LitStringPart -> string
    | Local -> keyword
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

let synonyms =
  [
    (".", "=>", false);
    ("«", "<<", false);
    ("¬", "!", false);
    ("»", ">>", false);
    ("Λ", "gen", false);
    ("λ", "fun", false);
    ("μ", "rec", false);
    ("„", ",,", true);
    ("→", "->", true);
    ("∀", "forall", false);
    ("∃", "exists", false);
    ("∧", "&&", true);
    ("∨", "||", true);
    ("≠", "!=", true);
    ("≤", "<=", true);
    ("≥", ">=", true);
    ("▷", "|>", true);
    ("◁", "<|", true);
    ("◇", "<>", true);
  ]
  |> List.map @@ fun (unicode, ascii, bop) ->
     object
       method unicode = unicode
       method ascii = ascii
       method bop = bop
     end
