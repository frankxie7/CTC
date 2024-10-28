(* Open the Graphics module *)
open Graphics

(* Main program execution *)
let () =
  (* Initialize the graphics system *)

  (* Create a window of size 640x480 *)

  (* Set the background color *)
  open_graph " 640x480";
  let background_color = rgb 208 181 154 in
  set_color background_color;

  fill_rect 0 0 (size_x ()) (size_y ());
  set_color blue;
  draw_circle 160 (size_y () / 2) 50;

  let current_hp = 80 in
  let scale = 1.0 in
  let light_brown = rgb 181 101 29 in
  set_color light_brown;

  let center_x = 200 in
  let center_y = 200 in

  let body_width = int_of_float (50.0 *. scale) in
  let body_height = int_of_float (27.0 *. scale) in
  let hump_width = int_of_float (22.0 *. scale) in
  let hump_height = int_of_float (25.0 *. scale) in
  let neck_radius = int_of_float (20.0 *. scale) in
  let neck_height = int_of_float (30.0 *. scale) in
  let leg_offset = int_of_float (15.0 *. scale) in
  let hind_leg_offset_x = int_of_float (-60.0 *. scale) in
  let hind_leg_offset_y = int_of_float (-8.0 *. scale) in
  let hp_box_height = int_of_float (15.0 *. scale) in
  let hp_bar_height = int_of_float (11.0 *. scale) in

  fill_ellipse center_x center_y body_width body_height;

  (* Camel body *)
  let hump_x = center_x in
  let hump_y = center_y + int_of_float (13.0 *. scale) in
  fill_ellipse hump_x hump_y hump_width hump_height;

  (* Camel hump *)
  set_line_width (int_of_float (19.0 *. scale));
  let neck_x = center_x + int_of_float (60.0 *. scale) in
  let neck_y = center_y + int_of_float (15.0 *. scale) in
  draw_arc neck_x neck_y neck_radius neck_height 225 360;

  (* Camel head *)
  let face_x = center_x + int_of_float (74.0 *. scale) in
  let face_y = center_y + int_of_float (37.0 *. scale) in
  let face_offset_x = int_of_float (37.5 *. scale) in
  let face_offset_y = int_of_float (24.0 *. scale) in
  let face_drop_small = int_of_float (12.5 *. scale) in
  let face_drop_large = int_of_float (5.0 *. scale) in
  fill_poly
    [|
      (face_x, face_y - face_drop_small);
      (face_x + (face_offset_x / 2), face_y - (face_drop_small / 2));
      (face_x + face_offset_x, face_y - face_drop_small);
      (face_x + face_offset_x, face_y - face_offset_y);
      (face_x, face_y - face_offset_y - face_drop_large);
    |];

  (* Camel foreleg *)
  let foreleg_x = center_x + int_of_float (25.0 *. scale) in
  let foreleg_y = center_y - leg_offset in
  fill_poly
    [|
      (foreleg_x, foreleg_y);
      (foreleg_x + int_of_float (13.0 *. scale), foreleg_y);
      ( foreleg_x + int_of_float (5.0 *. scale),
        foreleg_y - int_of_float (35.0 *. scale) );
      ( foreleg_x + int_of_float (3.0 *. scale),
        foreleg_y - int_of_float (73.0 *. scale) );
      ( foreleg_x + int_of_float (8.0 *. scale),
        foreleg_y - int_of_float (77.0 *. scale) );
      ( foreleg_x - int_of_float (6.0 *. scale),
        foreleg_y - int_of_float (77.0 *. scale) );
      ( foreleg_x - int_of_float (5.0 *. scale),
        foreleg_y - int_of_float (73.0 *. scale) );
      ( foreleg_x - int_of_float (4.0 *. scale),
        foreleg_y - int_of_float (35.0 *. scale) );
    |];

  (* Camel hindleg *)
  let hindleg_x = center_x + hind_leg_offset_x in
  let hindleg_y = center_y + hind_leg_offset_y in
  fill_poly
    [|
      (hindleg_x, hindleg_y);
      ( hindleg_x + int_of_float (25.0 *. scale),
        hindleg_y + int_of_float (24.0 *. scale) );
      ( hindleg_x + int_of_float (12.0 *. scale),
        hindleg_y - int_of_float (40.0 *. scale) );
      ( hindleg_x + int_of_float (14.0 *. scale),
        hindleg_y - int_of_float (53.0 *. scale) );
      ( hindleg_x + int_of_float (18.0 *. scale),
        hindleg_y - int_of_float (80.0 *. scale) );
      ( hindleg_x + int_of_float (22.0 *. scale),
        hindleg_y - int_of_float (85.0 *. scale) );
      ( hindleg_x + int_of_float (13.0 *. scale),
        hindleg_y - int_of_float (85.0 *. scale) );
      ( hindleg_x + int_of_float (2.0 *. scale),
        hindleg_y - int_of_float (45.0 *. scale) );
      ( hindleg_x - int_of_float (1.0 *. scale),
        hindleg_y - int_of_float (20.0 *. scale) );
      (hindleg_x, hindleg_y - int_of_float (15.0 *. scale));
    |];

  set_color white;

  moveto ((size_x () / 2) - 50) (size_y ());
  (* Center the text *)
  draw_string "Prototype Battle";

  fill_rect
    (center_x - int_of_float (65.0 *. scale))
    (center_y + int_of_float (60.0 *. scale))
    (int_of_float (151.0 *. scale))
    hp_box_height;

  set_color black;
  moveto
    (center_x - int_of_float (50.0 *. scale))
    (center_y + int_of_float (77.0 *. scale));
  draw_string ("HP: " ^ string_of_int current_hp ^ "/80");

  set_color red;
  let display_hp =
    int_of_float (145.0 *. scale /. 80.0 *. float_of_int current_hp)
  in
  fill_rect
    (center_x - int_of_float (62.0 *. scale))
    (center_y + int_of_float (62.0 *. scale))
    display_hp hp_bar_height;

  let _ = read_key () in
  close_graph ()
