open Tsdl
open Animations

type t = {
  mutable player : Camel.t;
  mutable enemy : Enemy.t;
}

val init_player : Camel.t -> Enemy.t -> t
val level_init : unit -> t

val draw_background :
  Sdl.renderer -> (* SDL renderer *)
                  Sdl.texture -> (* Background texture *)
                                 unit

val draw_camel_animation :
  t -> Sdl.renderer -> Sdl.texture -> Sdl.texture -> Sdl.texture -> unit

val draw_enemy_animation :
  t -> Sdl.renderer -> Sdl.texture -> Sdl.texture -> Sdl.texture -> unit

val init_bar : t -> Sdl.renderer -> unit
