open Tsdl

type t

val init_camel : unit -> t
val get_def : t -> int
val get_hp : t -> int
val get_energy : t -> int
val get_status : t -> (string * int) list
val get_animation : t -> string
val update_def : t -> int -> unit
val update_hp : t -> int -> unit
val update_energy : t -> int -> unit
val degrade_status : t -> unit
val update_status : t -> string -> unit
val update_animation : t -> string -> unit
