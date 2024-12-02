open Tsdl
open Animations

type t = {
  mutable player : Camel.t;
  mutable enemy : Enemy.t;
}

val init_player : Camel.t -> Enemy.t -> t
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
  unit

val draw_animation :
  t ->
  Sdl.renderer ->
  Sdl.texture ->
  Sdl.texture ->
  Sdl.texture ->
  string ref ->
  unit

val init_players_hp : t -> Sdl.renderer -> unit
