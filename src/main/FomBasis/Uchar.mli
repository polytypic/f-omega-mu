include module type of Stdlib.Uchar

val distances :
  pat:t array -> txt:t array -> pat_uc:t array -> txt_uc:t array -> int array
(** Computes a special array of distances between pattern `pat` and text `txt`.
 * `pat_uc` must be `pat` and `txt_uc` must be `txt` in uniform case.
 * At index 0 is the minimum distance over the whole text.
 * At index 1 is the index at which minimum distance was found.
 * At index 2 is the maximum possible distance depending only on pattern.
 * Starting at index 3 are distances between pattern and text.
 *)

val compare_distances : int array -> int array -> int
(** Compare distances computed by `distances`.  This is almost a lexicographic
 * comparison except that the shorter array is extended to match the longer
 * array as if the corresponding text would have continued with non-matching
 * characters.
 *)

val are_unrelated : int array -> bool
(** Determine whether distances seem to indicate pattern doesn't match text. *)
