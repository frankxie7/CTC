open Tsdl

type t = {
  name : string; (* Name of the animation *)
  col : int; (* Column index in the sprite sheet *)
  total_frames : int; (* Total number of frames in the animation *)
}
(* type representing an animation. *)

type animation = t
(* animation type *)

val animation_table : (string * animation) list
(** Table containing all animations. *)

val get_name : (string * animation) list -> string -> string
(** [get_name t] returns the name of the animation. *)

val get_col : (string * animation) list -> string -> int
(** [get_col t] returns the column index of the animation. *)

val get_frame_num : (string * animation) list -> string -> int
(** [get_frame_num t] returns the number of frames in the animation. *)

val animate : string -> int -> int -> t
(** [animate name col frame_num] creates a new animation. *)

val spit : animation
val idle : animation
val defend : animation
val stomp : animation

(* val camel_take_damage : int -> int -> int -> int -> Sdl.renderer -> unit (**
   [camel_take_damage row col src_width src_height r] Handles the camel's
   reaction when taking damage. - [row]: Starting row of the sprite. - [col]:
   Starting column of the sprite. - [src_width]: Width of the source rectangle.
   - [src_height]: Height of the source rectangle. - [r]: Renderer used for
   rendering. *) *)
