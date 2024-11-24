open Tsdl
open Tsdl_image
open Camel

let width = 1512
let height = 850

type t = {
  mutable player : Camel.t;
  mutable enemy : Enemy.t;
}

let bg_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:width ~h:height
let level_init () = { player = Camel.init_camel; enemy = Enemy.init_enemy }

let draw_level r bg_texture camel_texture source_width source_height =
  let bg_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:1920 ~h:1080 in
  Sdl.render_copy ~src:bg_rect ~dst:bg_rect r bg_texture |> Result.get_ok;
  draw_camel 0 0 source_width source_height r camel_texture

(* let draw_level r bg_rect sprite_rect t sprite_width sprite_height row_space
   col_space = (* Render background *) match Sdl.render_copy ~src:bg_rect
   ~dst:bg_rect r t with | Ok () -> () | Error (`Msg e) -> failwith ("Failed to
   render background: " ^ e);

   (* Render camel *) Camel.draw_camel 0 0 sprite_width sprite_height row_space
   col_space r t *)
