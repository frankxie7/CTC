open Tsdl.Sdl
open Tsdl_image
open Const

type m = {
  damage : int;
  defend : int;
  effect : string;
}

type t = {
  mutable hp : int;
  moves : m list;
}

let create_move (dmg : int) (def : int) (eff : string) : m =
  { damage = dmg; defend = def; effect = eff }

let hyena_moves = [ create_move 7 0 "None"; create_move 5 0 "None" ]
let create_enemy h m : t = { hp = h; moves = m }
let get_dmg m = m.damage
let init_enemy : t = { hp = 50; moves = hyena_moves }
let get_hp t = t.hp
let get_moves t = t.moves
let update_hp t x = t.hp <- t.hp - x

let draw_hyena r t =
  let src =
    Rect.create ~x:0 ~y:0 ~w:(enemy_width - 10) ~h:(frame_height - 10)
  in
  let dest = Rect.create ~x:enemy_x ~y:enemy_y ~w:175 ~h:175 in
  match render_copy ~src ~dst:dest r t with
  | Ok () -> ()
  | Error (`Msg e) -> failwith ("Failed to draw hyena: " ^ e)
