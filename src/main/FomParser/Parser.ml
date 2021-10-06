open FomBasis

module Error = struct
  open FomDiag

  type t = [Error.lexeme | Error.grammar | Error.duplicated_label]
end

let parse grammar lexer buffer : (_, [> Error.t], _) Rea.t =
  try
    lexer buffer
    |> MenhirLib.Convert.Simplified.traditional2revised grammar
    |> Rea.return
  with
  | Grammar.Error ->
    Rea.fail @@ `Error_grammar (Buffer.loc buffer, Buffer.lexeme_utf_8 buffer)
  | Lexer.Exn_lexeme (at, lexeme) -> Rea.fail @@ `Error_lexeme (at, lexeme)

let parse_utf_8 grammar lexer ?(path = "") input =
  Buffer.from_utf_8 ~path input |> parse grammar lexer
