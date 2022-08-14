open Rea
open StdlibPlus
open FomSource

exception Exn_lexeme of Loc.t * string

module Error = struct
  open FomError

  type t = [Error.lexeme | Error.grammar]
end

let parse grammar lexer buffer d =
  try
    lexer buffer
    |> pure'2 MenhirLib.Convert.Simplified.traditional2revised grammar
    |> run d
  with
  | Grammar.Error ->
    fail @@ `Error_grammar (Buffer.loc buffer, Buffer.lexeme_utf_8 buffer)
    |> run d
  | Exn_lexeme (at, lexeme) -> fail @@ `Error_lexeme (at, lexeme) |> run d

let parse_utf_8 grammar lexer ?(path = "") input =
  Buffer.from_utf_8 ~path input |> parse grammar lexer
