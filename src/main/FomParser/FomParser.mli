open FomBasis
open FomCST
open FomDiag

module Buffer : sig
  type t

  val from_utf_8 : ?path:string -> string -> t
  (** Create a new buffer from UTF-8 string. *)
end

module Lexer : sig
  type t

  val plain : t
  (** Lexical syntax without offside rules. *)

  val offside : t
  (** Lexical syntax with offside rules. *)

  module State : sig
    type t

    val initial : t
  end

  type token_info = {begins : int; ends : int; name : string; state : State.t}
  (** Describes a token for syntax highlighting purposes. *)

  val token_info_utf_8 : State.t -> string -> token_info
  (** Parse info of first single token from given UTF-8 string input. *)

  val offset_as_utf_16 : string -> int -> int
  (** Convert UTF-32 character offset to UTF-16 character offset with respect to given UTF-8 string. *)
end

module Grammar : sig
  type 'a t

  val program : Exp.t t
  (** Grammar of Fωμ expressions. *)

  val typ_exp : Typ.t t
  (** Grammar of Fωμ type expressions. *)

  val typ_defs : Typ.t Typ.Def.f list t
  (** Grammar of Fωμ type definitions. *)
end

module Parser : sig
  module Error : sig
    type t = [Error.lexeme | Error.grammar]
  end

  val parse : 'a Grammar.t -> Lexer.t -> Buffer.t -> ('r, [> Error.t], 'a) rea
  (** Parse from buffer using given grammar and lexical syntax. *)

  val parse_utf_8 :
    'a Grammar.t ->
    Lexer.t ->
    ?path:string ->
    string ->
    ('r, [> Error.t], 'a) rea
  (** Parse from UTF-8 string using given grammar and lexical syntax. *)
end
