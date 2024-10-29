type 'a t
(**['a t] is an abstract type representing a deck of card*)

exception Empty

val empty : 'a t
(**[empty] is the empty deck*)

val is_empty : 'a t -> bool
(**[is_empty] is true if the deck is equal to [empty]*)

val push : 'a -> 'a t -> 'a t
(**[push e d] is the deck d with element e eithin it*)
val peek : 'a t -> 'a
(**[peek d] is the top card of the deck [d]*)

val pop : 'a t -> 'a t
(**[pop d] is the deck [d] with its top card removed*)

val size : 'a t -> int
(**[size d] is the number of cards in the deck [d]*)

val to_list : 'a t -> 'a list
(**[to_list d] is the deck in list representation*)

val print : Card.t list -> unit
(**[print d] prints the deck representation for list.*)