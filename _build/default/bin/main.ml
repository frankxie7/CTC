open Graphics

let () =
  open_graph " 800x600";
  set_window_title "game start";
  let _ = read_key () in
  close_graph ()
