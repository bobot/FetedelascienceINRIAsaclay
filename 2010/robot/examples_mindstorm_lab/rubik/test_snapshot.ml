
open Graphics

let () =
  let w = Snapshot.start() in
  open_graph " 400x400-100+50";
  draw_image (make_image (Snapshot.take w)) 0 0;
  ignore (wait_next_event [Button_down]);
  Snapshot.stop w


(* Local Variables: *)
(* compile-command: "make -k test_snapshot.exe" *)
(* End: *)
