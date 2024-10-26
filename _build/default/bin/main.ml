<<<<<<< HEAD
open Graphics
=======
<<<<<<< HEAD
open Graphics

let () =
  open_graph " 800x600";
  set_window_title "game start";
  let _ = read_key () in
  close_graph ()
=======
<<<<<<< HEAD
(* simple_window.ml *)

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
=======
<<<<<<< HEAD
let () = print_endline "Starting main function..."
=======
let main () =
  (* Initialize the Gtk library *)
  let _ = GMain.init () in

  (* Create the main window *)
  let window =
    GWindow.window ~title:"Simple Window" ~border_width:10 ~width:400
      ~height:200 ()
  in

  (* Show the window *)
  window#show ();

  (* Start the GTK main loop *)
  GMain.main ()

(* Run the main function *)
let () = main ()
>>>>>>> refs/remotes/origin/main
>>>>>>> refs/remotes/origin/main
>>>>>>> refs/remotes/origin/main
>>>>>>> refs/remotes/origin/main
