type t = int array

val distances :
  pat:'c array -> txt:'c array -> pat_uc:'c array -> txt_uc:'c array -> t
(** Computes a special array of distances between pattern `pat` and text `txt`.
 * `pat_uc` must be `pat` and `txt_uc` must be `txt` in uniform case.
 * At index 0 is the minimum distance over the whole text.
 * At index 1 is the index at which minimum distance was found.
 * At index 2 is the maximum possible distance depending only on pattern.
 * Starting at index 3 are distances between pattern and text.
 *)

val compare : t -> t -> int
(** Compare distances computed by `distances`.  This is almost a lexicographic
 * comparison except that the shorter array is extended to match the longer
 * array as if the corresponding text would have continued with non-matching
 * characters.
 *)

val are_unrelated : t -> bool
(** Determine whether distances seem to indicate pattern doesn't match text. *)
