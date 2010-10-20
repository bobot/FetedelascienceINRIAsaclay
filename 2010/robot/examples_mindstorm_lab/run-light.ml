open Printf
module Sensor = Mindstorm.Sensor
module Motor = Mindstorm.Motor

module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =
struct
  (* Le robot roule en suivant une ligne verte au sol (sur fond blanc).
     S'il voit de la couleur noire, il fait un quart de tour à droite
     et continue de suivre la ligne verte *)

  (* Initialisation, at functor instantiation *)
  let () =
    Sensor.set C.conn `S2 `Light_active `Pct_full_scale;
    let stop _ =
      Motor.set C.conn Motor.all (Motor.speed 0);
      Mindstorm.close C.conn;
      printf "\n";
      exit 0 in
    Sys.set_signal Sys.sigint (Sys.Signal_handle stop);
    printf "Press Ctrl-c to quit.\n%!"
  ;;

  let speed motor ?tach_limit sp =
    Motor.set C.conn motor (Motor.speed ?tach_limit (-sp))

  let r = Robot.make()
  let light = Robot.meas r (fun () -> (Sensor.get C.conn `S2).Sensor.scaled)
  let idle = Robot.meas r (fun () ->
                             let (state,_,_,_) = Motor.get C.conn Motor.b in
                             state.Motor.run_state = `Idle)

  let black a = a < 28
  let green a = (a < 45 && a > 33)
  let white a = a > 45

  let rec turn tl sp =
    Robot.event light (fun a -> (green a)) (fun _ -> go_straight());
    Robot.event light (fun a -> (black a)) go_straightBeforeTurn90;
    Robot.event idle (fun v -> v) (fun _ -> turn (tl*2) (-sp));
    speed Motor.b ~tach_limit:tl sp;
    speed Motor.c ~tach_limit:tl (-sp)

  and go_straight () =
    let sp = if Random.bool() then 20 else -20 in
    Robot.event light (fun a -> (white a)) (fun _ -> turn 40 sp);
    Robot.event light (fun a -> (black a)) go_straightBeforeTurn90;
    speed Motor.b 50;
    speed Motor.c 50

  and go_straightBeforeTurn90 _ =
    Robot.event idle (fun v -> v) (turn90deg 200 20);
    speed Motor.b ~tach_limit:130 25;
    speed Motor.c ~tach_limit:130 25

  and turn90deg tl sp _ =
    Robot.event idle (fun v -> v) (fun _ -> go_straight());
    speed Motor.b ~tach_limit:tl sp;
    speed Motor.c ~tach_limit:tl (-sp)

  let run() =
    go_straight ();
    Robot.run r

end

let () =
  let bt =
    if Array.length Sys.argv < 2 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);
      exit 1;
    )
    else Sys.argv.(1) in
  let conn = Mindstorm.connect_bluetooth bt in
  let module R = Run(struct let conn = conn end) in
  R.run()
