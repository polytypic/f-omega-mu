open FomDiag

let parse grammar lexer buffer =
  try lexer buffer |> MenhirLib.Convert.Simplified.traditional2revised grammar
  with Grammar.Error ->
    Error.syntax (Buffer.loc buffer) (Buffer.lexeme_utf_8 buffer)

let parse_utf_8 grammar lexer ?(filename = "") input =
  Buffer.from_utf_8 ~filename input |> parse grammar lexer
