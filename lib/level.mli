open Tsdl
open Animations

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

val draw_animation :
  Sdl.renderer ->
  (string, Animations.t) Hashtbl.t ->
  string ->
  int ->
  int ->
  int ->
  int ->
  int ->
  Sdl.texture ->
  unit
(** [draw_animation r animation_table anim_name frame_count src_width src_height dst_x dst_y t]
    Draws the specified animation on the screen.

    - [r]: The SDL renderer used for rendering.
    - [animation_table]: A hash table mapping animation names to their
      definitions.
    - [anim_name]: The name of the animation to draw.
    - [frame_count]: The current frame count, used to determine the animation
      frame to render.
    - [src_width]: The width of a single frame in the source sprite sheet.
    - [src_height]: The height of a single frame in the source sprite sheet.
    - [dst_x]: The x-coordinate of the destination rectangle where the animation
      will be drawn.
    - [dst_y]: The y-coordinate of the destination rectangle where the animation
      will be drawn.
    - [t]: The texture containing the sprite sheet. **)

val init_players_hp : t -> Sdl.renderer -> unit
