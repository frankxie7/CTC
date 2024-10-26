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
