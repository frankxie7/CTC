type m
(**[m] is an abstract type representing a move*)

type t
(**[t] is an abstract type representing an enemy with hp and moves*)

val create_enemy : int -> m list -> t
val create_move : int -> int -> string -> m
