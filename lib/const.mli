val screen_width : int
(* width dimension of the display = 1512 *)

val screen_height : int
(* height dimension of the display = 850*)

val camel_x : int
(* x coordinate of where the camel should go on the display *)

val camel_y : int
(* y coordinate of the where the camel should go on the display *)

val frame_height : int
(* height of each frame on the sprite sheet*)

val frame_width : int
(* width of each frame on the sprite sheet*)

val frame_height_scaling : int
(* scaling of the each frame's height scaling on the sprite sheet to the
   display *)

val frame_width_scaling : int
(*scaling of the frame's width on the sprite sheet to the display *)

val init_width : int
(* distance between side of the png to the left side of the first frame. *)

val init_height : int
(* distance between the top of th png to the top of the first frame*)

val camel_max_hp : int
(* camel's max health *)

val snake_max_hp : int

(* enemy's max health *)
val bear_max_hp : int
val human_max_hp : int
val enemy_hp_x : int
(* enemy x location *)

val enemy_hp_y : int
(* enemy y location*)

val enemy_x : int
val enemy_y : int
val camel_max_energy : int
