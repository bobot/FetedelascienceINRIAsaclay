open Printf
module Sensor = Mindstorm.Sensor
module Motor = Mindstorm.Motor

let usleep s = ignore (Unix.select [] [] [] s)

let run conn =
  let set_speed ?(sync=false) a b  =
    Motor.set conn Motor.a (Motor.speed a);
    Motor.set conn Motor.b (Motor.speed b) 
  in
  let stop _ = set_speed 0 0; Mindstorm.close conn; exit 0 in
  Sys.set_signal Sys.sigint (Sys.Signal_handle stop);

  let pause () = set_speed 0 0; usleep 1. in
  (* 8.5cm *)
  let forward n = 
    set_speed ~sync:true 100 100;
    usleep n;
    set_speed 0 0
  in
  let turn sign = 
    set_speed 100 (-100);
    usleep 0.61;
    set_speed 0 0
  in
  while true do 
(*    forward 2.5;
    pause ();  *)
    turn 1;
    pause ()
  done 

let () =
  let bt =
    if Array.length Sys.argv < 2 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);
      exit 1;
    )
    else Sys.argv.(1) in
  let conn = Mindstorm.connect_bluetooth bt in
  run conn
