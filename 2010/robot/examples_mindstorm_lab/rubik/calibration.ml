open Graphics
open Init_color
open Printf
open Snapshot

type color = int

let calibrate_webcam ?(snapshot_file="/Users/marku/Desktop/ocaml.ppm") _ =
  (* let wc = Snapshot.start () in *)
  (* let img = Snapshot.take wc in *)
  let img = Ppm.as_matrix_exn snapshot_file in
  let height = Array.length img
  and width = Array.length img.(0) in
  open_graph (sprintf " %ix%i"  width height);
  set_color (rgb 0 255 242);
  print_endline "press 'q' to quit and another key for refresh";
  let fill_checking_zone x y =
    let col = Pick.average( Pick.pick_point img (Pick.abs x) (Pick.ord y)) in

    (* for checking the fiability of the detection color module *)
    printf "%s " (Color.to_string (Color.name col));

    let (a,b,c) = col in
    set_color (rgb a b c);
    fill_rect (Pick.abs x) (Pick.ord y) 14 14;
    set_color (rgb 0 255 242);
    draw_rect (Pick.abs x) (Pick.ord y) 14 14 in
  (* refreshing the snapshot cubie.*)
  let rec refresh () =
    (* take a new snapshot *)
    (* let img = Snapshot.take wc in *)
    let img = Ppm.as_matrix_exn snapshot_file in
    draw_image (make_image img) 0 0;
    Array.iter (fun x ->


                  (* for checking the fiability of the detection color module *)
                  printf "\n%!";

                  Array.iter (fun y -> fill_checking_zone x y) [|0;1;2|]
               ) [|0;1;2|];
    printf "\n%!";
    if not (read_key() = 'q') then refresh ()
  in refresh ();
  close_graph ()

let calibrate_mechanics _ =
  ()

let () =
  calibrate_webcam ()
