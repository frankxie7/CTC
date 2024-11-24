open Tsdl

type t

val camel_x : int
val camel_y : int
val create_camel : int -> int -> string -> t
val init_camel : t
val get_hp : t -> int
val get_energy : t -> int
val get_status : t -> string

val draw_camel :
  int ->
  (* row *)
  int ->
  (* col *)
  int ->
  (* row_space *)
  int ->
  (* col_space *)
  Sdl.renderer ->
  (* SDL renderer *)
  Sdl.texture ->
  (* SDL texture *)
  unit
