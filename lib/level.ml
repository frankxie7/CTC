open Tsdl
open Tsdl_image
open Camel
open Enemy
open Animations
open Const

let init_anim = idle
let init_y = 7
let calculate_frame frame_count total_frames = frame_count mod total_frames

type t = {
  mutable player : Camel.t;
  mutable enemy : Enemy.t;
}

type players = t

let init_player (p : Camel.t) (e : Enemy.t) : players =
  { player = p; enemy = e }

let bg_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:screen_width ~h:screen_height
let level_init () = { player = Camel.init_camel; enemy = Enemy.init_enemy }

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

let init_energy_bar x y curr_energy max_energy r : unit =
  let max_energy = max_energy * 33 in
  let curr_energy = curr_energy * 33 in
  Tsdl.Sdl.set_render_draw_color r 0 255 0 0 |> ignore;
  let border_rect =
    Sdl.Rect.create ~x:(x - 4) ~y:(y - 4) ~w:(max_energy + 8) ~h:24
  in
  Tsdl.Sdl.render_fill_rect r (Some border_rect) |> ignore;
  Tsdl.Sdl.set_render_draw_color r 255 255 255 0 |> ignore;
  let max_rect = Sdl.Rect.create ~x ~y ~w:max_energy ~h:16 in
  Tsdl.Sdl.render_fill_rect r (Some max_rect) |> ignore;
  if curr_energy > 2 * max_energy / 3 then
    Tsdl.Sdl.set_render_draw_color r 0 128 0 255 |> ignore
  else if curr_energy > max_energy / 3 then
    Tsdl.Sdl.set_render_draw_color r 255 255 0 255 |> ignore
  else Tsdl.Sdl.set_render_draw_color r 255 0 0 255 |> ignore;
  let curr_rect = Sdl.Rect.create ~x ~y ~w:curr_energy ~h:16 in
  Tsdl.Sdl.render_fill_rect r (Some curr_rect) |> ignore;
  Tsdl.Sdl.set_render_draw_color r 255 255 255 255 |> ignore

let init_players_eng (t : players) r : unit =
  init_energy_bar (camel_x + 60) (camel_y - 75) (Camel.get_hp t.player)
    camel_max_hp r;
  init_energy_bar (enemy_x + 45) (enemy_y - 45) (Enemy.get_hp t.enemy)
    enemy_max_hp r

let init_players_hp (t : players) r : unit =
  init_hp_bar (camel_x + 20) (camel_y - 25) (Camel.get_hp t.player) camel_max_hp
    r;
  init_hp_bar (enemy_x + 15) (enemy_y - 15) (Enemy.get_hp t.enemy) enemy_max_hp
    r

let draw_level r bg_texture camel_texture hyena_texture =
  print_string "Drawing background and enemy units.\n";
  let bg_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:screen_width ~h:screen_height in
  Sdl.render_copy ~src:bg_rect ~dst:bg_rect r bg_texture |> Result.get_ok;
  (* draw_camel r camel_texture; *)
  draw_hyena r hyena_texture

let draw_animation state renderer bg_texture camel_texture enemy_texture anim =
  print_string "Drawing camel animation. \n";
  let anim_name = Animations.get_anim anim in
  let total_frames =
    Animations.get_frame_num Animations.animation_table anim_name
  in
  print_string
    ("Animation has " ^ string_of_int total_frames ^ " frames on column "
    ^ string_of_int (Animations.get_col Animations.animation_table anim_name - 1)
    ^ ". \n");
  for current_frame = 0 to total_frames - 1 do
    draw_level renderer bg_texture camel_texture enemy_texture;
    let col = Animations.get_col Animations.animation_table anim_name in
    let src_rect =
      Sdl.Rect.create
        ~x:(camel_init_width + (frame_width * (col - 1)))
        ~y:(camel_init_height + (current_frame * frame_height))
        ~w:frame_width ~h:frame_height
    in
    let dst_rect =
      Sdl.Rect.create ~x:camel_x ~y:camel_y ~w:camel_width_scaling
        ~h:camel_height_scaling
    in
    Sdl.render_copy ~src:src_rect ~dst:dst_rect renderer camel_texture
    |> Result.get_ok;
    init_players_hp state renderer;
    Sdl.render_present renderer;
    Tsdl.Sdl.delay (Int32.of_int 100)
  done;
  Animations.set_anim anim "idle"

(* TODO : check if frame_count is less than total_frame number *)
