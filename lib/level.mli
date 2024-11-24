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
  (* Camel texture *)
  Sdl.texture ->
  (* Background texture *)
  int ->
  int ->
  unit
