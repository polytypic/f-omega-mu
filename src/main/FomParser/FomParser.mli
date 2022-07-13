open StdlibPlus
open FomCST
open FomError

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

  val is_id : string -> bool
  (** Determine whether the string would be a valid id. *)

  val is_id_or_nat : string -> bool
  (** Determine whether the string would be a valid id or natural number. *)

  val is_nat : string -> bool
  (** Determine whether the string would be a valid natural number. *)

  val coerce_to_id : string -> string
  (** Translate arbitrary UTF-8 string to valid id character by character. *)
end

module Tokenizer : sig
  module State : sig
    type t

    val initial : t
  end

  type token_info = {begins : int; ends : int; name : string; state : State.t}
  (** Describes a token for syntax highlighting purposes. *)

  val token_info_utf_8 : State.t -> string -> token_info
  (** Parse info of first single token from given UTF-8 string input. *)

  val offset_as_utf_16 : string -> int -> int
  (** Convert UTF-32 character offset to UTF-16 character offset with respect to
      given UTF-8 string. *)

  val offset_as_utf_32 : string -> int -> int
  (** Convert UTF-16 character offset to UTF-32 character offset with respect to
      given UTF-8 string. *)

  val synonyms : < unicode : string ; ascii : string ; bop : bool > list
  (** List of unicode symbols and their ascii alternatives. *)

  val keywords : string list
  (** List of keywords. *)

  val pervasives : string list
  (** List of identifiers implicitly available in every module. *)

  val identifiers : string -> string Seq.t
  (** Parse a list of all identifiers in the given string. *)
end

module Grammar : sig
  type 'a t

  val mods : Exp.t t
  (** Grammar of Fωμ expressions or modules. *)

  val sigs : Typ.t t
  (** Grammar of Fωμ type expressions or signatures. *)

  val incs : Typ.t Typ.Defs.f t
  (** Grammar of Fωμ type definitions or includes. *)
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
