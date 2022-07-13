open StdlibPlus
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
  type t = bool * Buffer.state

  let initial = (false, [])
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
    | BraceLhsNS -> punctuation
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
    | DoubleAngleQuoteLhsNS -> punctuation
    | DoubleAngleQuoteRhs -> punctuation
    | DoubleComma -> operator
    | EOF -> error
    | Ellipsis -> punctuation
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
    | Semicolon -> punctuation
    | Slash -> operator
    | Star -> operator
    | Target -> keyword
    | Then -> keyword
    | Tick -> punctuation
    | TriangleLhs -> punctuation
    | TriangleRhs -> punctuation
    | TstrClose -> string
    | TstrEsc _ -> string
    | TstrOpen _ -> string
    | TstrOpenRaw -> string
    | TstrStr _ -> string
    | TstrStrPart -> string
    | Type -> keyword
    | Underscore -> variable
  and to_deltas = function
    | TstrOpenRaw | TstrOpen _ -> (0, 1)
    | TstrEsc _ -> (-1, 0)
    | _ -> (0, 0)
  in
  fun (in_string, state) ->
    let lexer = if in_string then string_continuation else token_or_comment in
    fun input ->
      let buffer = Buffer.from_utf_8 input in
      buffer.state <- state;
      match lexer buffer with
      | token, lhs, rhs ->
        let db, de = to_deltas token in
        {
          begins = lhs.pos_cnum + db;
          ends = rhs.pos_cnum + de;
          name = to_name token;
          state = (token = TstrStrPart, buffer.state);
        }

(* *)

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
    ("…", "...", false);
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

(* *)

module StringSet = Set.Make (String)

let keywords =
  [
    "case";
    "else";
    "exists";
    "forall";
    "fun";
    "gen";
    "if";
    "import";
    "in";
    "include";
    "let";
    "local";
    "rec";
    "target";
    "then";
    "type";
  ]

let pervasives = ["and"; "bool"; "false"; "impure"; "int"; "string"; "true"]

let identifiers input =
  let buffer = Buffer.from_utf_8 input in
  let rec loop ids =
    match Res.catch @@ fun () -> token_or_comment buffer with
    | `Error _ | `Ok (EOF, _, _) -> StringSet.to_seq ids
    | `Ok (Id id, _, _ | IdTyp id, _, _) -> loop (StringSet.add id ids)
    | `Ok _ -> loop ids
  in
  loop StringSet.empty
