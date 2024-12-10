open Tsdl

type t = {
  name : string; (* Name of the animation *)
  col : int; (* Column index in the sprite sheet *)
  total_frames : int; (* Total number of frames in the animation *)
}
(* type representing an animation. *)

type animation = t
(* animation type *)

val camel_animation_table : (string * animation) list
(** Table containing all camel animations. *)

val snake_animation_table : (string * animation) list
(** Table containing all snake animations. *)

val get_name : (string * animation) list -> string -> string
(** [get_name t] returns the name of the animation. *)

val get_col : (string * animation) list -> string -> int
(** [get_col t] returns the column index of the animation. *)

val get_frame_num : (string * animation) list -> string -> int
(** [get_frame_num t] returns the number of frames in the animation. *)

val animate : string -> int -> int -> t
(** [animate name col frame_num] creates a new animation. *)
