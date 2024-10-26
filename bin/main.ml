(* Open the Graphics module *)
open Graphics

let () =
  (* Initialize the graphics system *)
  open_graph " 640x480";

  (* Create a window of size 640x480 *)

  (* Set the background color *)
  set_window_title "Simple Window";
  set_color black;
  fill_rect 0 0 (size_x ()) (size_y ());

  (* Fill the window with black color *)

  (* Draw some text *)
  set_color white;
  moveto ((size_x () / 2) - 50) (size_y () / 2);
  (* Center the text *)
  draw_string "Hello, OCaml Graphics!";

  (* Wait for a mouse click before closing *)
  let _ = read_key () in
  close_graph () (* Close the window *)
