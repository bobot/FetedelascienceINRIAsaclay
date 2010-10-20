open Printf
module Motor = Mindstorm.Motor

let switch_port = `S1
let motor_left = Motor.a
let motor_right = Motor.b
let dir = -1

module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =
struct
  (* Le robot va tout droit; s'il touche un obstacle, il s'arrête. *)

  let r = Robot.make()
  let touch = Robot.touch C.conn switch_port r

  let rec stop _ =
    Motor.set C.conn Motor.all (Motor.speed 0)

  and go_straight() =
    Robot.event_is touch stop;
    Motor.set C.conn motor_left  (Motor.speed (30 * dir) ~sync:true);
    Motor.set C.conn motor_right (Motor.speed (30 * dir) ~sync:true)

  let run() =
    go_straight();
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
  printf "Press the button on the robot to stop.\n%!";
  R.run()
