open Printf
module Motor = Mindstorm.Motor

let button_port = `S1
let ultrasonic_port = `S4
let motor_left = Motor.a
let motor_right = Motor.b

module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =
struct
  (* Le robot roule et s'il voit un mur, il ralentit, et il tourne. *)

  let r = Robot.make()

  (* Initialize and create a measure for the ultrasonic sensor. *)
  let ultra =
    let u = Mindstorm.Sensor.Ultrasonic.make C.conn ultrasonic_port in
    Mindstorm.Sensor.Ultrasonic.set u `Meas_cont;
    Robot.meas r (fun _ -> Mindstorm.Sensor.Ultrasonic.get u `Byte0)

  let button_pushed =
    Mindstorm.Sensor.set C.conn button_port `Switch `Bool;
    Robot.meas r (fun _ ->
                    let v = Mindstorm.Sensor.get C.conn button_port in
                    v.Mindstorm.Sensor.scaled = 1)

  let stop () =
    Motor.set C.conn Motor.all (Motor.speed 0);
    raise Exit

  let rec turn _ =
    Robot.event ultra (fun d -> d > 50) go_straight;
    Robot.event_is button_pushed stop;
    Motor.set C.conn motor_left (Motor.speed (-30));
    Motor.set C.conn motor_right (Motor.speed 30)

  and go_straight dist =
    Robot.event ultra (fun d -> d < 40) turn;
    Robot.event_is button_pushed stop;
    Motor.set C.conn motor_left (Motor.speed ((-dist)/2));
    Motor.set C.conn motor_right (Motor.speed ((-dist)/2))

  let run() =
    go_straight 90;
    Robot.run r
end;;

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
