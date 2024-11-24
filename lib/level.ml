open Tsdl
open Tsdl_image
open Camel
open Enemy

let width = 1512
let height = 850

type t = {
  mutable player : Camel.t;
  mutable enemy : Enemy.t;
}

type players = t

let bg_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:width ~h:height
let level_init () = { player = Camel.init_camel; enemy = Enemy.init_enemy }

let draw_level r bg_texture camel_texture hyena_texture src_width src_height =
  let bg_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:1920 ~h:1080 in
  Sdl.render_copy ~src:bg_rect ~dst:bg_rect r bg_texture |> Result.get_ok;
  draw_camel 0 0 src_width src_height r camel_texture;
  draw_hyena 0 0 src_width src_height r hyena_texture

let init_hp_bar x y curr_health max_health r : unit =
  let max_health = max_health * 3 in
  let curr_health = curr_health * 3 in
  Tsdl.Sdl.set_render_draw_color r 0 0 0 255 |> ignore;
  let border_rect =
    Sdl.Rect.create ~x:(x - 4) ~y:(y - 4) ~w:(max_health + 8) ~h:24
  in
  Tsdl.Sdl.render_fill_rect r (Some border_rect) |> ignore;
  Tsdl.Sdl.set_render_draw_color r 255 255 255 0 |> ignore;
  let max_rect = Sdl.Rect.create ~x ~y ~w:max_health ~h:16 in
  Tsdl.Sdl.render_fill_rect r (Some max_rect) |> ignore;
  if curr_health > 2 * max_health / 3 then
    Tsdl.Sdl.set_render_draw_color r 0 128 0 255 |> ignore
  else if curr_health > max_health / 3 then
    Tsdl.Sdl.set_render_draw_color r 255 255 0 255 |> ignore
  else Tsdl.Sdl.set_render_draw_color r 255 0 0 255 |> ignore;
  let curr_rect = Sdl.Rect.create ~x ~y ~w:curr_health ~h:16 in
  Tsdl.Sdl.render_fill_rect r (Some curr_rect) |> ignore;
  Tsdl.Sdl.set_render_draw_color r 255 255 255 255 |> ignore

let init_players_hp (t : players) r : unit =
  init_hp_bar camel_x camel_y (Camel.get_hp t.player) (Camel.get_hp init_camel)
    r;
  init_hp_bar (enemy_x + 15) (enemy_y - 15) (Enemy.get_hp t.enemy)
    (Enemy.get_hp init_enemy) r
