module UTF8 : sig
  val to_uchar_array : string -> Uchar.t array
  (** Convert UTF-8 string to an array of Unicode characters. *)

  val of_uchar_array : Uchar.t array -> string
  (** Convert an array of Unicode characters to UTF-8 string. *)
end

module UTF16 : sig
  val of_uchar_array : Uchar.t array -> bytes
  val to_uchar_array : bytes -> Uchar.t array
end
