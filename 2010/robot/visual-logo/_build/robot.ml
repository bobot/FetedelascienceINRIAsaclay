open Printf
module Sensor = Mindstorm.Sensor
module Motor  = Mindstorm.Motor 

let usleep s = ignore (Unix.select [] [] [] s)

let bt = "00:16:53:0A:AF:E0"
(*  if Array.length Sys.argv < 2 then (
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1)  *)

let conn = Mindstorm.connect_bluetooth bt 

let string_of_color = function
  | `Black -> "black"
  | `Blue -> "blue"
  | `Green -> "green"
  | `Red -> "red"
  | `White -> "white" 
  | `Yellow -> "yellow" 

let eye      = `S1
let ()       = Sensor.set conn eye `Color_full `Pct_full_scale
let rec repeat n f = if n = 0 then [] else f () :: repeat (n - 1) f
let max_cardinal l = 
  let t = Hashtbl.create 13 in 
  List.iter (fun x -> 
	       try Hashtbl.replace t x (Hashtbl.find t x + 1) 
	       with Not_found -> Hashtbl.add t x 1) l;
  let (max_k, _) = 
    Hashtbl.fold (fun k v (max_k, max_v) ->
		    if v > max_v then (k, v) else (max_k, max_v))
      t 
      (`Black, -1)
  in
  max_k 

let color () = 
  let samples = repeat 5 (fun () -> Sensor.color_of_data (Sensor.get conn eye)) in 
  let c = max_cardinal samples in
  Printf.printf "See %s\n%!" (string_of_color c);
  c

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

let freeze () = Motor.set conn Motor.all (Motor.speed 0)

let stop _ = freeze (); Mindstorm.close conn; exit 0 

let () = Sys.set_signal Sys.sigint (Sys.Signal_handle stop)

let pause () = freeze ()

let forward_coef = 0.05
let forward_speed = 40

(* let forward_precise nb_tach = *)
(*   Motor.reset_pos conn Motor.a; *)
(*   Motor.reset_pos conn Motor.b;  *)
(*   pause (); *)
(*   if debug then print_motor_state (); *)
(*   let tach = get_rot_a () in *)
(*   let tach_limit = tach + nb_tach in *)
(*   let state = Motor.speed ~tach_limit forward_speed in *)
(*   Format.printf "Run %i@." state.Motor.tach_limit; *)
(*   Motor.set conn Motor.a state; *)
(*   Motor.set conn Motor.b state; *)
(*   while not (is_idle ()) do *)
(*       print_motor_state () *)
(*   done; *)
(*   Format.printf "Reached %i@." (get_tach_a ()); *)
(*   let state = {state with Motor.run_state = `Idle; *)
(* 	      Motor.motor_on = false} in *)
(*   Motor.set conn Motor.a state; *)
(*   Motor.set conn Motor.b state; *)
(*   Format.printf "Stopped@."; *)
(*   if debug then print_motor_state () *)

let speed motor ?tach_limit sp =
  Motor.set conn motor (Motor.speed ?tach_limit sp)

let forward_precise nb_tach =
  Motor.reset_pos conn Motor.a;
  Motor.reset_pos conn Motor.b; 
  pause ();
  let abs = abs nb_tach in
  print_motor_state ();
  let tach = get_rot_a () in
  let tach_limit = abs in
  Format.printf "Run %i %i@." tach tach_limit;
  speed Motor.a ~tach_limit forward_speed;
  speed Motor.b ~tach_limit forward_speed;
  while not (is_idle ()) do
    print_motor_state ();
  done;
  pause ()

let degree_coef = 0.05
let turn_speed = 20

let turn_precise degree =
  Motor.reset_pos conn Motor.a;
  Motor.reset_pos conn Motor.b; 
  pause ();
  let abs = abs degree in
  let sgn = if degree < 0 then -1 else 1 in
  print_motor_state ();
  let tach = get_rot_a () in
  let tach_limit = abs in
  Format.printf "Run %i %i@." tach tach_limit;
  speed Motor.a ~tach_limit (-sgn * turn_speed);
  speed Motor.b ~tach_limit (sgn * turn_speed);
  while not (is_idle ()) do
    print_motor_state ()
  done;
  pause ()

let quarter_turn_degree = 500
let forward_step = 500

let tourne_gauche () = 
  turn_precise (-quarter_turn_degree)

let tourne_droite () = 
  turn_precise quarter_turn_degree

let tourne deg = 
  turn_precise (deg * quarter_turn_degree / 90)

let avance () = 
  forward_precise forward_step

let avance_un_peu () = 
  forward_precise (forward_step / 20)

let repete (n, what) = 
  for i = 0 to n - 1 do 
    what ()
  done

let rouge = `Red

let blanc = `White

let noir = `Black

exception Found
let cherche_non_noir () = 
  let turn_check_and_return d = 
    turn_precise d;
    if color () <> `Black then raise Found;
    turn_precise (-d);
    if color () <> `Black then raise Found
  in
  try
    if color () = `Black then begin
      for i = 1 to 8 do
        let j = i * 10 + 60 in
	turn_check_and_return j;
	turn_check_and_return (-j)
      done;
      failwith "Perdu!"
    end
  with Found -> ()

let couleur_est c = 
  color () = c

let catch_road () = 
  while 
    couleur_est (blanc)
    || couleur_est (noir)
  do
    while 
      couleur_est (blanc)
    do  
      avance_un_peu ()
    done;
    cherche_non_noir ()
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


let turn_to dir =
  forward_precise 350;
  match dir with
    | `Up -> ()
    | `Left -> tourne (-90)
    | `Right -> tourne (90)
    | _ -> ()

let goto_next dir = 
  turn_to dir;
  catch_road ()

let rec follow_iti from current = function
  | dest :: next ->
      let dir = choose_dir from current dest in
      Printf.printf "%d -> %d -> %d : %s\n%!" from current dest (string_of_dir dir);
      goto_next dir;
      follow_iti current dest next
  | [] -> 
      stop ()
  
