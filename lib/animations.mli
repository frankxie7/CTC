open Tsdl
(** Animation module *)

type t = {
  name : string;  (** Name of the animation *)
  col : int;  (** Column index in the sprite sheet *)
  frame_num : int;  (** Number of frames in the animation *)
}
(** Type representing an animation. *)

type animation = t
(** Alias for the animation type. *)

val get_name : t -> string
(** [get_name t] returns the name of the animation. *)

val get_col : t -> int
(** [get_col t] returns the column index of the animation. *)

val get_frame_num : t -> int
(** [get_frame_num t] returns the number of frames in the animation. *)

val col_space : int
(** Distance between each column of frames in the sprite sheet. *)

val row_space : int

val animate : string -> int -> int -> t
(** [animate name col frame_num] creates a new animation. *)

val spit : animation
(** Predefined animations. *)

val idle : animation
val defend : animation
val stomp : animation

val animation_table : (string, animation) Hashtbl.t
(** Table containing all animations. *)

val camel_take_damage : int -> int -> int -> int -> Sdl.renderer -> unit
(** [camel_take_damage row col src_width src_height r] Handles the camel's
    reaction when taking damage.
    - [row]: Starting row of the sprite.
    - [col]: Starting column of the sprite.
    - [src_width]: Width of the source rectangle.
    - [src_height]: Height of the source rectangle.
    - [r]: Renderer used for rendering. *)
