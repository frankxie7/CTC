open Tsdl.Sdl
open Tsdl_image

type m = {
  name : string;
  damage : int;
  defend : int;
  effect : string;
}

type t = {
  mutable hp : int;
  moves : m array;
  mutable animation : string;
}

val get_name : m -> string
(** [get_name m] returns the name of the move [m]. *)

val get_dmg : m -> int
(** [get_dmg m] returns the damage value of the move [m]. *)

val create_move : string -> int -> int -> string -> m
(** [create_move n dmg def eff] creates a move with name [n], damage [dmg],
    defense [def], and effect [eff]. *)

val snake_moves : m array
(** An array of moves for the snake enemy. *)

val bear_moves : m array
(** An array of moves for the bear enemy. *)

val man_moves : m array
(** An array of moves for the human enemy. *)

val create_enemy : int -> m array -> t
(** [create_enemy h m] creates an enemy with health [h] and a list of moves [m]. *)

val init_snake : unit -> t
(** [init_snake ()] initializes a snake enemy. *)

val init_bear : unit -> t
(** [init_bear ()] initializes a bear enemy. *)

val init_man : unit -> t
(** [init_man ()] initializes a human enemy. *)

val get_hp : t -> int
(** [get_hp t] returns the current health points of the enemy [t]. *)

val get_moves : t -> m array
(** [get_moves t] returns the list of moves of the enemy [t]. *)

val get_animation : t -> string
(** [get_animation t] returns the current animation state of the enemy [t]. *)

val update_animation : t -> string -> unit
(** [update_animation t x] updates the animation state of the enemy [t] to [x]. *)

val update_hp : t -> int -> unit
(** [update_hp t x] reduces the health points of the enemy [t] by [x]. *)
