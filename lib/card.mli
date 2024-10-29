type t
(**[t] is an abstract type representing a card*)

val basicA : t
(**[basicA] is a basic attack card*)

val basicD : t
(**[basicD] is a basic defend card*)

val basicAD : t
(**[basicAD] is a basic attack and defend card*)

val print_card : t -> unit
