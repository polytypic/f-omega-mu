open FomBasis
open Grammar

let[@warning "-32"] to_string = function
  | And -> "and"
  | ArrowRight -> "→"
  | BraceLhs -> "{"
  | BraceRhs -> "}"
  | BracketLhs -> "["
  | BracketLhsNS -> "["
  | BracketRhs -> "]"
  | Caret -> "^"
  | Case -> "case"
  | Colon -> ":"
  | Comma -> ","
  | Comment _ -> "# ..."
  | Diamond -> "◇"
  | Dot -> "."
  | DoubleAngleQuoteLhs -> "«"
  | DoubleAngleQuoteRhs -> "»"
  | EOF -> "<EOF>"
  | Else -> "else"
  | Equal -> "="
  | Exists -> "∃"
  | ForAll -> "∀"
  | Greater -> ">"
  | GreaterEqual -> "≥"
  | Id id -> id
  | IdDollar id -> id
  | IdSub id -> id
  | IdTyp id -> id
  | If -> "if"
  | Import -> "import"
  | In -> "in"
  | Include -> "include"
  | LambdaLower -> "λ"
  | LambdaUpper -> "Λ"
  | Less -> "<"
  | LessEqual -> "≤"
  | Let -> "let"
  | Local -> "local"
  | LitNat n -> Bigint.to_string n
  | LitString s -> JsonString.to_utf8_json s
  | LitStringPart -> "..."
  | LogicalAnd -> "∧"
  | LogicalNot -> "∨"
  | LogicalOr -> "¬"
  | Minus -> "-"
  | MuLower -> "μ"
  | NotEqual -> "≠"
  | ParenLhs -> "("
  | ParenLhsNS -> "("
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