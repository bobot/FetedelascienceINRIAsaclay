open Printf
module Sensor = Mindstorm.Sensor
module Motor  = Mindstorm.Motor 

let usleep s = ignore (Unix.select [] [] [] s)

let bt =
  if Array.length Sys.argv < 2 then (
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1) 

let conn = Mindstorm.connect_bluetooth bt 

let eye      = `S1
let ()       = Sensor.set conn eye `Color_full `Raw
let color () = Sensor.color_of_data (Sensor.get conn eye)

let red () = 
  Sensor.set conn eye `Color_red `Raw;
  usleep 1.;
  (Sensor.get conn eye).Sensor.scaled

let blue () = 
  Sensor.set conn eye `Color_blue `Raw;
  usleep 1.;
  (Sensor.get conn eye).Sensor.scaled

let green () = 
  Sensor.set conn eye `Color_green `Raw;
  usleep 1.;
  (Sensor.get conn eye).Sensor.scaled


let string_of_color = function
  | `Black -> "black"
  | `Blue -> "blue"
  | `Green -> "green"
  | `Red -> "red"
  | `White -> "white" 
  | `Yellow -> "yellow" 

let debug = true

let print_motor_state () =
  let (_,a_tach_count, a_block_tach_count, a_rotation_count) = Motor.get conn Motor.a in
  let (_,b_tach_count, b_block_tach_count, b_rotation_count) = Motor.get conn Motor.a in
  Format.printf "@[<hov>I'm at @[<hov>a = (%i,%i,%i)@\nb = (%i,%i,%i)@]@]@."
    a_tach_count a_block_tach_count a_rotation_count
    b_tach_count b_block_tach_count b_rotation_count

let get_tach_a () =
  let (_,a_tach_count, _, _) = Motor.get conn Motor.a in
  a_tach_count

let get_rot_a () =
  let (_,_, _, r) = Motor.get conn Motor.a in
  r

let is_idle () =
  let (state, _, _, _) = Motor.get conn Motor.a in
  state.Motor.run_state = `Idle


let set_speed ?(sync=false) a b  =
  if debug then print_motor_state ();
  Motor.set conn Motor.a (Motor.speed a);
  Motor.set conn Motor.b (Motor.speed b);
  if debug then print_motor_state ()

let freeze () = set_speed 0 0

let stop _ = freeze (); Mindstorm.close conn; exit 0 

let () = Sys.set_signal Sys.sigint (Sys.Signal_handle stop)

let pause () = freeze (); usleep 1.

let forward_coef = 0.05
let forward_speed = 100

let forward n =
  Printf.printf "forward %f\n%!" n;
  set_speed ~sync:true forward_speed forward_speed;
  usleep (n *. forward_coef);
  Printf.printf "forward %f done\n%!" n;
  freeze ()


let forward_precise nb_tach =
  if debug then print_motor_state ();
  let tach = get_tach_a () in
  let tach_limit = tach + nb_tach in
  let state = Motor.speed ~tach_limit forward_speed in
  Format.printf "Run %i@." state.Motor.tach_limit;
  Motor.set conn Motor.a state;
  Motor.set conn Motor.b state;
(*  usleep (0.5);
  Format.printf "Run@.";
  if debug then print_motor_state ();
  let tach_limit = tach + nb_tach in
  let state = Motor.speed ~tach_limit ~sync:true 0 in
  let state = {state with Motor.run_state = `Ramp_down} in
  Motor.set conn Motor.a state;
  Motor.set conn Motor.b state;
  Format.printf "Reached %i@." tach_limit;*)
  while not (is_idle ()) do
      print_motor_state ()
  done;
  Format.printf "Reached %i@." (get_tach_a ());
  let state = {state with Motor.run_state = `Idle;
	      Motor.motor_on = false} in
  Motor.set conn Motor.a state;
  Motor.set conn Motor.b state;
  Format.printf "Stopped@.";
  if debug then print_motor_state ()


let degree_coef = 0.05
let turn_speed = 20

let turn degree = 
  let abs = abs_float degree in
  let sgn = if degree < 0. then -1 else 1 in
  set_speed ~sync:true (sgn * turn_speed) ((-sgn) * turn_speed);
  usleep (abs *. degree_coef);
  freeze ()

let speed motor ?tach_limit sp =
  Motor.set conn motor (Motor.speed ?tach_limit sp)

let turn_precise degree = 
  let abs = abs degree in
  let sgn = if degree < 0 then -1 else 1 in
  print_motor_state ();
  let tach = get_tach_a () in
  let tach_limit = tach + abs in
  Format.printf "Run %i %i@." tach tach_limit;
  speed Motor.a ~tach_limit (sgn * turn_speed);
  speed Motor.b ~tach_limit (-sgn * turn_speed);
  while not (is_idle ()) do
    print_motor_state ();
    usleep 0.1;
  done;
  freeze ()


let max_recatching_angle = 30.
let recatching_speed = 3.
let recatching_step = 3.

(* Recatch_path : -1, +1, -2, +2 ... *)
let recatch_path () = 
  let turned_degree = ref 0. in
  let current_max_degree = ref 0. in
  let direction = ref recatching_speed in
  while not (color () = `White || color () = `Red) do
    Printf.eprintf "Turned: %f | Color : %s\n%!" 
      !turned_degree (string_of_color (color ()));
    turn !direction;
    turned_degree := !turned_degree +. !direction;
    if (abs_float (!turned_degree) > max_recatching_angle) then 
      begin
	Printf.eprintf "I'm lost!";
	stop ()
      end;
    if (abs_float (!turned_degree) > !current_max_degree) then
      begin
	Printf.eprintf "I try the other direction : Turned: %f%!" 
	  !turned_degree;
	current_max_degree := abs_float (!turned_degree) +. recatching_step;
	direction := -. !direction;
	turn (-.2.*. !turned_degree);
	turned_degree := -. !turned_degree;
      end;

  done

let map = [|
  [| -1; -1; -1; -1 |];
  [| 4; 2; -1; -1 |];
  [| 5; 3; -1; 1 |];
  [| 6; -1; -1; 2 |];
  [| 7; 5; 1; -1 |];
  [| 8; 6; 2; 4 |];
  [| 9; -1; 3; 5 |];
  [| 10; 8; 4; -1 |];
  [| 11; 9; 5; 7 |];
  [| 12; -1; 6; 8 |];
  [| -1; 11; 7; -1 |];
  [| 12; -1; 8; 1 |];
  [| -1; -1; 9; 11 |]
|]

let next_of c = map.(c)

let next_next n i =
  n.((i + 1) mod 4)

let next_dir = function
  | `Down -> `Right
  | `Right -> `Up
  | `Up -> `Left
  | `Left -> `Down

let dir_of_pos = function
  | 0 -> `Right
  | 1 -> `Up
  | 2 -> `Left
  | 3 -> `Down
  | _ -> assert false

let pos_of_dir = function
  | `Right -> 0
  | `Up -> 1
  | `Left -> 2
  | `Down -> 3
  | _ -> assert false

let string_of_dir = function
  | `Right -> "Right"
  | `Left -> "Left"
  | `Up -> "Up"
  | `Down -> "Down"

exception Found_int of int
exception Not_found_int of int

let find_pos next c = 
  try 
    Array.iteri (fun i x -> 
		   Printf.printf "%d => %d\n" i x;
		   if x = c then raise (Found_int i)) next;
    raise (Not_found_int c)
  with Found_int i -> i
  

let choose_dir from current dest = 
  let next = next_of current in
  let pos  = ref (find_pos next from) in 
  Printf.printf "pos = %d\n%!" !pos;
  let dir  = ref `Down in 
  let node = ref from in
  while (!node <> dest) do
    dir  := next_dir !dir;
    node := next_next next !pos;
    pos  := !pos + 1;
    Printf.printf "%s %d %d\n%!" (string_of_dir !dir) !node !pos;
    if !node = from then raise Not_found
  done;
  !dir
  
let turn_on_track_dist = 13.

let turn_on_track dir = 
  forward turn_on_track_dist;
  match dir with
    | `Down -> turn 90.
    | `Right -> turn 18.
    | `Up -> turn 0.
    | `Left -> turn (-.18.)

let follow_speed = 2.

let rec follow_path () = 
  recatch_path ();
  while (color () = `White) do
    forward follow_speed
  done;
  if color () <> `Red then follow_path ()

let go dir = 
  turn_on_track dir;
  follow_path ()

let rec follow_iti from current = function
  | dest :: next ->
      let dir = choose_dir from current dest in
      Printf.printf "%d -> %d -> %d : %s\n%!" from current dest (string_of_dir dir);
      go dir;
      follow_iti current dest next
  | [] -> 
      stop ()

let () = 
(*  Motor.reset_pos conn Motor.a;
  Motor.reset_pos conn Motor.b; *)
  forward_precise 1000;
(*  follow_path () *)
(*  Motor.reset_pos conn Motor.a;
  Motor.reset_pos conn Motor.b;
  usleep 5.;
  Format.printf "Start@.";
  forward_precise 1 *)
