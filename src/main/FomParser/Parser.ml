open FomBasis

let parse grammar lexer buffer =
  try
    lexer buffer
    |> MenhirLib.Convert.Simplified.traditional2revised grammar
    |> Rea.return
  with
  | Grammar.Error ->
    Rea.fail @@ `Error_grammar (Buffer.loc buffer, Buffer.lexeme_utf_8 buffer)
  | Lexer.Exn_lexeme (at, lexeme) -> Rea.fail @@ `Error_lexeme (at, lexeme)
  | FomCST.Exn_duplicated_label (at, label) ->
    Rea.fail @@ `Error_duplicated_label (at, label)

let parse_utf_8 grammar lexer ?(filename = "") input =
  Buffer.from_utf_8 ~filename input |> parse grammar lexer
