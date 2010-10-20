open Printf
open Graphics

let snapshot_file = "/Users/marku/Desktop/ocaml.ppm"
(* let snapshot_file = "/tmp/ocaml.ppm" *)

let take_snapshot () =
  Sys.command ("gqcam -w 0 -c 128 -b 255 --type PPM --dump " ^ snapshot_file)


let rgb_components c =
  float((c lsr 16) land 0xFF),
  float((c lsr 8) land 0xFF),
  float(c land 0xFF)

let rgb (r, g, b) =
  ((truncate r land 0XFF) lsl 16) lor ((truncate g land 0xFF) lsl 8)
  lor (truncate b land 0xFF)

let map f img =
  Array.map (fun row -> Array.map (fun c -> rgb(f (rgb_components c))) row) img


let () =
  (* take_snapshot(); *)
  let img = Ppm.as_matrix_exn snapshot_file in
  let height = Array.length img
  and width = Array.length img.(0) in
  open_graph (sprintf " %ix%i" (4 * width) height);

  let f1 (r,g,b) = (r,0.,0.)
  and f2 (r,g,b) = (0.,g,0.)
  and f3 (r,g,b) = (0.,0.,b) in
  draw_image (make_image img) 0 0;
  draw_image (make_image (map f1 img)) width 0;
  draw_image (make_image (map f2 img)) (2*width) 0;
  draw_image (make_image (map f3 img)) (3*width) 0;

  print_endline "press a key";
  ignore(read_key());
  close_graph()
