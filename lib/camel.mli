open Tsdl

type t

val create_camel : int -> int -> string -> t
val init_camel : t
val get_hp : t -> int
val get_energy : t -> int
val get_status : t -> string
val update_hp : t -> int -> unit
val update_energy : t -> int -> unit
val update_status : t -> string -> unit

val draw_camel :
  Sdl.renderer -> (* SDL renderer *)
                  Sdl.texture -> (* SDL texture *)
                                 unit
