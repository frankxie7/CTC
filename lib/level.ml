open Tsdl
open Tsdl_image
open Camel
open Enemy
open Animations
open Const

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

let level_init () =
  { player = Camel.init_camel (); enemy = Enemy.init_snake () }

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

let init_energy_bar (t : players) r : unit =
  let x = camel_x + 20 in
  let y = camel_y - 10 in

  let max_length = camel_max_hp * 3 in
  let scale = max_length / camel_max_energy in
  let curr_energy = Camel.get_energy t.player in

  for z = 0 to camel_max_energy - 1 do
    Tsdl.Sdl.set_render_draw_color r 0 0 0 0 |> ignore;
    let energy_border =
      Sdl.Rect.create ~x:(x + (z * scale) - 4) ~y ~w:(scale + 8) ~h:18
    in
    Tsdl.Sdl.render_fill_rect r (Some energy_border) |> ignore;

    if z < curr_energy then Tsdl.Sdl.set_render_draw_color r 0 49 83 0 |> ignore
    else Tsdl.Sdl.set_render_draw_color r 220 220 220 0 |> ignore;

    let energy_bar =
      Sdl.Rect.create ~x:(x + (z * scale)) ~y:(y + 4) ~w:scale ~h:10
    in
    Tsdl.Sdl.render_fill_rect r (Some energy_bar) |> ignore
  done

let init_bar (t : players) r (level : int) : unit =
  let enemy_hp =
    if level = 1 then snake_max_hp
    else if level = 2 then bear_max_hp
    else if level = 3 then human_max_hp
    else failwith "Invalid level"
  in
  let camel_hp =
    if Camel.get_hp t.player < 0 then 0 else Camel.get_hp t.player
  in
  let enemy_curr_hp =
    if Enemy.get_hp t.enemy < 0 then 0 else Enemy.get_hp t.enemy
  in
  init_hp_bar (camel_x + 20) (camel_y - 25) camel_hp camel_max_hp r;
  init_hp_bar (enemy_hp_x + 15) (enemy_hp_y - 15) enemy_curr_hp enemy_hp r;
  init_energy_bar t r

let draw_background r bg_texture =
  let bg_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:screen_width ~h:screen_height in
  Sdl.render_copy ~src:bg_rect ~dst:bg_rect r bg_texture |> Result.get_ok

let draw_enemy_base r t =
  let src_rect =
    Sdl.Rect.create ~x:(init_width + frame_width) ~y:init_height ~w:frame_width
      ~h:frame_height
  in
  let dst_rect =
    Sdl.Rect.create ~x:enemy_x ~y:enemy_y ~w:frame_width_scaling
      ~h:frame_height_scaling
  in
  match Sdl.render_copy ~src:src_rect ~dst:dst_rect r t with
  | Ok () -> ()
  | Error (`Msg e) -> failwith ("Failed to draw enemy: " ^ e)

let draw_camel_base r t =
  let src_rect =
    Sdl.Rect.create ~x:(init_width + frame_width) ~y:init_height ~w:frame_width
      ~h:frame_height
  in
  let dst_rect =
    Sdl.Rect.create ~x:camel_x ~y:camel_y ~w:frame_width_scaling
      ~h:frame_height_scaling
  in
  match Sdl.render_copy ~src:src_rect ~dst:dst_rect r t with
  | Ok () -> ()
  | Error (`Msg e) -> failwith ("Failed to draw base camel: " ^ e)

let draw_enemy_animation state renderer bg_texture camel_texture enemy_texture
    level =
  let animation_table =
    match level with
    | 1 -> Animations.snake_animation_table
    | 2 -> Animations.bear_animation_table
    | 3 -> Animations.human_animation_table
    | _ -> failwith "Animation table error"
  in
  let anim_name = Enemy.get_animation state.enemy in
  let total_frames = Animations.get_frame_num animation_table anim_name in
  for current_frame = 0 to total_frames - 1 do
    draw_background renderer bg_texture;
    draw_camel_base renderer camel_texture;
    let col = Animations.get_col animation_table anim_name in
    let src_rect =
      Sdl.Rect.create
        ~x:(init_width + (frame_width * (col - 1)))
        ~y:(init_height + (current_frame * frame_height))
        ~w:frame_width ~h:frame_height
    in
    let dst_rect =
      Sdl.Rect.create ~x:enemy_x ~y:enemy_y ~w:frame_width_scaling
        ~h:frame_height_scaling
    in
    Sdl.render_copy ~src:src_rect ~dst:dst_rect renderer enemy_texture
    |> Result.get_ok;
    init_bar state renderer level;
    Sdl.render_present renderer;
    let x = Sdl.wait_event_timeout None 500 in
    if x then () else ();
    Sdl.delay (Int32.of_int 100)
  done;
  Enemy.update_animation state.enemy "idle"

let draw_camel_animation state renderer bg_texture camel_texture enemy_texture
    level =
  let anim_name = Camel.get_animation state.player in
  let total_frames =
    Animations.get_frame_num Animations.camel_animation_table anim_name
  in
  for current_frame = 0 to total_frames - 1 do
    draw_background renderer bg_texture;
    draw_enemy_base renderer enemy_texture;
    let col = Animations.get_col Animations.camel_animation_table anim_name in
    let src_rect =
      Sdl.Rect.create
        ~x:(init_width + (frame_width * (col - 1)))
        ~y:(init_height + (current_frame * frame_height))
        ~w:frame_width ~h:frame_height
    in
    let dst_rect =
      Sdl.Rect.create ~x:camel_x ~y:camel_y ~w:frame_width_scaling
        ~h:frame_height_scaling
    in
    Sdl.render_copy ~src:src_rect ~dst:dst_rect renderer camel_texture
    |> Result.get_ok;
    init_bar state renderer level;

    Sdl.render_present renderer;
    let x = Sdl.wait_event_timeout None 500 in
    if x then () else ();
    Sdl.delay (Int32.of_int 100)
  done;

  Camel.update_animation state.player "idle"
