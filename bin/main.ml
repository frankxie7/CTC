(* Open the Graphics module *)
open Graphics

let () =
  (* Initialize the graphics system *)
  let () = open_graph " 640x480" in

  (* Create a window of size 640x480 *)

  (* Set the background color *)
  set_window_title "Game Prototype";
  set_color black;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color blue;
  draw_circle 160 (size_y () / 2) 50;

  (* Fill the window with black color *)

  (* Draw some text *)
  set_color white;
  moveto ((size_x () / 2) - 50) (size_y ());
  (* Center the text *)
  draw_string "Prototype Battle";

  (* Wait for a mouse click before closing *)
  let _ = read_key () in
  close_graph () (* Close the window *)
