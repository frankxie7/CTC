open Tsdl.Sdl
open Tsdl_image
open Const

type m = {
  name : string;
  damage : int;
  defend : int;
  effect : string;
}

let get_name m = m.name
let get_dmg m = m.damage

let create_move (n : string) (dmg : int) (def : int) (eff : string) : m =
  { name = n; damage = dmg; defend = def; effect = eff }

(* let snake_moves = [ create_move "bite" 7 0 "Bleed"; create_move "flick" 5 0
   "None" ] *)

(* let bear_moves = [ create_move "growl" 15 0 "None"; create_move "maul" 15 0
   "None" ] *)
let snake_moves = [| create_move "bite" 7 0 "Bleed" |]
let bear_moves = [| create_move "flick" 5 0 "None" |]
let man_moves = [| create_move "mohan" 10 1 "None" |]
(*create_move "chuck" 10 1 "None";*)

type t = {
  mutable hp : int;
  moves : m array;
  mutable animation : string;
}

let create_enemy h m : t = { hp = h; moves = m; animation = "idle" }
let init_snake () : t = { hp = 10; moves = snake_moves; animation = "idle" }
let init_bear () : t = { hp = 10; moves = bear_moves; animation = "idle" }
let init_man () : t = { hp = 10; moves = man_moves; animation = "idle" }
let get_hp t = t.hp
let get_moves t = t.moves
let get_animation t = t.animation
let update_animation t x = t.animation <- x
let update_hp t x = t.hp <- t.hp - x

let print_movelist (moves : m list) =
  List.iter (fun move -> Printf.printf "Move Name: %s\n" move.name) moves

let draw_enemy_base r t =
  let src_rect =
    Rect.create ~x:(init_width + frame_width) ~y:init_height ~w:frame_width
      ~h:frame_height
  in
  let dst_rect =
    Rect.create ~x:enemy_x ~y:enemy_y ~w:frame_width_scaling
      ~h:frame_height_scaling
  in
  match render_copy ~src:src_rect ~dst:dst_rect r t with
  | Ok () -> ()
  | Error (`Msg e) -> failwith ("Failed to draw enemy: " ^ e)
