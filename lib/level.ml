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

let draw_animation r animation_table anim_name frame_count texture =
  (* TODO : check if frame_count is less than total_frame number *)
  match Hashtbl.find_opt animation_table anim_name with
  | Some animation ->
      (* TODO : check if this works - col is not completely implemented yet but
         it should either be (1) 1-2-3 representing the col of the animation or
         the actual y coordinates of the animation. *)
      let src_rect =
        Sdl.Rect.create
          ~x:(camel_init_height + frame_height)
          ~y:(camel_init_width + (frame_width * (frame_count - 1)))
          ~w:camel_width ~h:camel_height
      in
      let dst_rect =
        Sdl.Rect.create ~x:camel_x ~y:camel_y ~w:camel_width_scaling
          ~h:camel_height_scaling
      in
      Sdl.render_copy ~src:src_rect ~dst:dst_rect r texture |> Result.get_ok
  | None -> failwith ("Animation not found: " ^ anim_name)

type players = t

let bg_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:screen_width ~h:screen_height
let level_init () = { player = Camel.init_camel; enemy = Enemy.init_enemy }

let draw_level r bg_texture camel_texture hyena_texture =
  let bg_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:screen_width ~h:screen_height in
  Sdl.render_copy ~src:bg_rect ~dst:bg_rect r bg_texture |> Result.get_ok;
  (* draw_camel r camel_texture; *)
  draw_hyena r hyena_texture

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
  init_hp_bar (camel_x + 20) (camel_y - 25) (Camel.get_hp t.player) camel_max_hp
    r;
  init_hp_bar (enemy_x + 15) (enemy_y - 15) (Enemy.get_hp t.enemy) enemy_max_hp
    r
