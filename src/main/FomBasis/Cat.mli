type 'a t

val empty : 'a t
val singleton : 'a -> 'a t
val append : 'a t -> 'a t -> 'a t
val to_list : 'a t -> 'a list
