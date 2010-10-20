(* File : run.ml *)
open Printf

module Motor = Mindstorm.Motor

let conn = let bt =
  if Array.length Sys.argv < 2 then (
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1) in Mindstorm.connect_bluetooth bt

module C =
  struct
    let conn = conn
    let light_port = `S3
    let ultra_port = `S4
    let switch_port1 = `S1
    let switch_port2 = `S2
    let motor_ultra = Motor.c
    let motor_left = Motor.a
    let motor_right = Motor.b

    module Labyrinth = Display.Make(Labyrinth)
  end

module Solver = Solver.Make(C)

let () =
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> Solver.stop()));
  printf "Press Ctrl-c to quit.\n%!";
  (* Define the strategy to follow *)
  let rec solve () =
    Solver.follow_path look (Solver.next_square_to_explore())
  and look () =
    Solver.look_walls solve
  in
  Solver.look_back look;
  Solver.run_loop();
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> Solver.stop()));
  printf "Press Ctrl-c to quit.\n%!"
