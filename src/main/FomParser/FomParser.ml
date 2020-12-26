module Buffer = Buffer
module Lexer = Lexer

module Grammar = struct
  include Grammar

  type 'a t = (Lexing.lexbuf -> token) -> Lexing.lexbuf -> 'a
end

include Parser
