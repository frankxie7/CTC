open Tsdl

type t

val init_camel : unit -> t
(** [init_camel ()] initializes a new camel with default attributes. *)

val get_def : t -> int
(** [get_def t] returns the current defense value of the camel [t]. *)

val get_hp : t -> int
(** [get_hp t] returns the current health points (HP) of the camel [t]. *)

val get_energy : t -> int
(** [get_energy t] returns the current energy level of the camel [t]. *)

val get_status : t -> (string * int) list
(** [get_status t] returns the list of status effects currently affecting the
    camel [t], where each status is represented as a tuple of the status name
    and its remaining duration. *)

val get_animation : t -> string
(** [get_animation t] returns the current animation state of the camel [t] as a
    string. *)

val update_def : t -> int -> unit
(** [update_def t x] updates the defense value of the camel [t] by [x]. The
    value [x] can be positive (increasing defense) or negative (decreasing
    defense). *)

val update_hp : t -> int -> unit
(** [update_hp t x] reduces the health points (HP) of the camel [t] by [x]. The
    value [x] should be a positive integer. *)

val update_energy : t -> int -> unit
(** [update_energy t x] modifies the energy level of the camel [t] by [x]. The
    value [x] can be positive (increasing energy) or negative (decreasing
    energy). *)

val degrade_status : t -> unit
(** [degrade_status t] decreases the remaining duration of all active status
    effects on the camel [t] by 1 and removes any status effects that have
    expired. *)

val update_status : t -> string -> unit
(** [update_status t s] adds or updates the status effect [s] for the camel [t].
    If [s] is already present, it resets its duration to the default value;
    otherwise, it adds [s] to the list of active status effects. *)

val update_animation : t -> string -> unit
(** [update_animation t x] sets the animation state of the camel [t] to [x]. The
    value [x] should be a string representing the desired animation state. *)
