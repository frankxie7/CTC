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

let snake_moves =
  [ create_move "bite" 7 0 "Bleed"; create_move "flick" 5 0 "None" ]

let wolf_moves = [ create_move "bite" 15 0 "None" ]
let man_moves = [ create_move "nuke" 400000 40000 "None" ]

type t = {
  mutable hp : int;
  moves : m list;
  mutable animation : string;
}

let create_enemy h m : t = { hp = h; moves = m; animation = "idle" }
let init_snake () : t = { hp = 50; moves = snake_moves; animation = "idle" }
let init_wolf () : t = { hp = 1; moves = wolf_moves; animation = "idle" }
let init_man () : t = { hp = 1; moves = man_moves; animation = "idle" }
let get_hp t = t.hp
let get_moves t = t.moves
let get_animation t = t.animation
let update_animation t x = t.animation <- x
let update_hp t x = t.hp <- t.hp - x

let draw_enemy_base r t =
  let src_rect =
    Rect.create
      ~x:(camel_init_width + camel_width)
      ~y:camel_init_height ~w:camel_width ~h:frame_height
  in
  let dst_rect =
    Rect.create ~x:camel_x ~y:camel_y ~w:camel_width_scaling
      ~h:camel_height_scaling
  in
  match render_copy ~src:src_rect ~dst:dst_rect r t with
  | Ok () -> ()
  | Error (`Msg e) -> failwith ("Failed to draw enemy: " ^ e)
