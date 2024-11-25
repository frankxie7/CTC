open Tsdl

type t = {
  mutable player : Camel.t;
  mutable enemy : Enemy.t;
}

val level_init : unit -> t

val draw_level :
  Sdl.renderer ->
  (* SDL renderer *)
  Sdl.texture ->
  (* Background texture *)
  Sdl.texture ->
  (* Camel texture *)
  Sdl.texture ->
  (* Hyena texture *)
  int ->
  int ->
  unit

val init_players_hp : t -> Sdl.renderer -> unit
