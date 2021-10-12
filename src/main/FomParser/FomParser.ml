module Buffer = Buffer
module Lexer = Lexer
module Tokenizer = Tokenizer

module Grammar = struct
  include Grammar

  type 'a t = (Lexing.lexbuf -> token) -> Lexing.lexbuf -> 'a
end

module Parser = Parser
