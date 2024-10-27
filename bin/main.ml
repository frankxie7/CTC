(* Open the Graphics module *)
open Graphics

let () =
  let () = open_graph " 640x480" in
  let background_color = rgb 208 181 154 in
  set_color background_color;
  fill_rect 0 0 (size_x ()) (size_y ());

  let light_brown = rgb 181 101 29 in
  set_color light_brown;
  let camel_center_x = 150 in
  let camel_center_y = 175 in
  fill_ellipse camel_center_x camel_center_y 50 27;
  (* Camel body *)
  fill_ellipse camel_center_x (camel_center_y + 10) 22 25;
  (* Camel hump *)
  set_line_width 19;
  draw_arc (camel_center_x + 60) (camel_center_y + 15) 20 30 225 360;
  (* Camel neck *)
  fill_ellipse (camel_center_x + 84) (camel_center_y + 27) 12 10;
  let head_center_x = 224 in
  let head_center_y = 211 in
  fill_poly
    [|
      (head_center_x, head_center_y);
      (head_center_x + 40, head_center_y - 4);
      (head_center_x + 40, head_center_y - 16);
      (head_center_x, head_center_y - 24);
    |];
  (* Camel head *)
  let leg_center_one_x = camel_center_x + 25 in
  let leg_center_one_y = camel_center_y - 15 in

  fill_poly
    [|
      (leg_center_one_x, leg_center_one_y);
      (leg_center_one_x + 13, leg_center_one_y);
      (leg_center_one_x + 5, leg_center_one_y - 35);
      (leg_center_one_x + 3, leg_center_one_y - 73);
      (leg_center_one_x + 8, leg_center_one_y - 77);
      (leg_center_one_x - 6, leg_center_one_y - 77);
      (leg_center_one_x - 5, leg_center_one_y - 73);
      (leg_center_one_x - 4, leg_center_one_y - 35);
    |];

  let leg_center_two_x = camel_center_x - 60 in
  let leg_center_two_y = camel_center_y - 8 in
  fill_poly
    [|
      (leg_center_two_x, leg_center_two_y);
      (leg_center_two_x + 25, leg_center_two_y + 24);
      (leg_center_two_x + 12, leg_center_two_y - 40);
      (leg_center_two_x + 14, leg_center_two_y - 53);
      (leg_center_two_x + 18, leg_center_two_y - 80);
      (leg_center_two_x + 22, leg_center_two_y - 85);
      (leg_center_two_x + 13, leg_center_two_y - 85);
      (leg_center_two_x + 2, leg_center_two_y - 45);
      (leg_center_two_x - 1, leg_center_two_y - 20);
      (leg_center_two_x, leg_center_two_y - 15);
    |];

  set_color white;
  fill_rect 95 235 150 15;
  set_color black;
  moveto 100 252;
  let hp = 80 in
  draw_string ("HP: " ^ string_of_int hp ^ "/80");
  set_color red;
  fill_rect 98 237 140 11;

  (* Set the background color *)
  (* set_window_title "Simple Window"; set_color black; fill_rect 0 0 (size_x
     ()) (size_y ());

     (* Fill the window with black color *)

     (* Draw some text *) set_color white; moveto ((size_x () / 2) - 50) (size_y
     () / 2); (* Center the text *) draw_string "Hello, OCaml Graphics!"; *)

  (* Wait for a mouse click before closing *)
  let _ = read_key () in
  close_graph () (* Close the window *)
