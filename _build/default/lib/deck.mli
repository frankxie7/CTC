type 'a t

exception Empty

val empty : 'a t
val is_empty : 'a t -> bool
val push : 'a -> 'a t -> 'a t
val peek : 'a t -> 'a
val pop : 'a t -> 'a t
val size : 'a t -> int
val to_list : 'a t -> 'a list
