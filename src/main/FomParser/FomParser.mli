open FomSyntax

module Buffer : sig
  type t

  val from_utf_8 : ?filename:string -> string -> t
  (** Create a new buffer from UTF-8 string. *)
end

module Lexer : sig
  type t

  val plain : t
  (** Lexical syntax without offside rules. *)

  type token_info = {begins : int; ends : int; name : string}
  (** Describes a token for syntax highlighting purposes. *)

  val token_info_utf_8 : string -> token_info
  (** Parse info of first single token from given UTF-8 string input. *)
end

module Grammar : sig
  type 'a t

  val program : Exp.t t
  (** Grammar of Fωμ expressions. *)

  val typ_exp : Typ.t t
  (** Grammar of Fωμ type expressions. *)
end

val parse : 't Grammar.t -> Lexer.t -> Buffer.t -> 't
(** Parse from buffer using given grammar and lexical syntax. *)

val parse_utf_8 : 't Grammar.t -> Lexer.t -> ?filename:string -> string -> 't
(** Parse from UTF-8 string using given grammar and lexical syntax. *)
