(* File: solver.ml

   Copyright (C) 2008

     Julie de Pril <julie_87@users.sourceforge.net>

     Christophe Troestler <chris_77@users.sourceforge.net>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


open Mindstorm

(* We parametrise the solver by the connections to the ports (so as to
   change them easily) and by the Labyrinth implementation (so as to
   enrich it esily with e.g. a graphical mode). *)
module Make(C: sig
              val conn : Mindstorm.bluetooth Mindstorm.conn
              val motor_left : Motor.port
              val motor_right : Motor.port
              val motor_ultra : Motor.port
              val light_port : Sensor.port
              val ultra_port : Sensor.port
              val switch_port1 : Sensor.port
              val switch_port2 : Sensor.port

              module Labyrinth : Display.T
            end) =
struct
  open C
  open Printf

  (** Stops all the motors and switches off the light sensor. *)
  let stop ?music () =
    Motor.set conn Motor.all (Motor.speed 0);
    Sensor.set conn light_port `Light_inactive `Pct_full_scale;
    (match music with
    | None -> ()
    | Some file -> (try Mindstorm.Sound.play C.conn file with _ -> ()));
    Unix.sleep 3;
    Mindstorm.close conn;
    (* Leave the graph displayed (=> do not exit immediately) *)
    Labyrinth.close_when_clicked();
    exit 0

  (** If the robot finds the exit of the labyrinth, it will call this
      function. *)
  let found_exit () =
    Labyrinth.success();
    printf "found issue\n%!";
    stop () ~music:"! Fanfare.rso"

  (** If the robot determines that there is no exit to the labyrinth,
      it will call this function. *)
  let no_exit_exists () =
    Labyrinth.failure();
    printf "no issue\n%!";
    stop () ~music:"Ahnoo.rso"

  (** Continuations taken by the fonctions. *)
  type cont = unit -> unit

  (** Search the closest visited squares satisfying a condition
   ***********************************************************************)

  (* Node and the path from the stating node leading to it *)
  module CoordAndPath =
  struct
    type t = Labyrinth.Coord.t * Labyrinth.dir list
        (* A position in the labyrinth with an associated path saying
           how to reach it. *)

    let compare (p,_) (q,_) = Labyrinth.Coord.compare p q
      (* We only care about not visiting twice the same position, the
         paths do not matter. *)
  end

  module S = struct
    include Set.Make(CoordAndPath)
    let mem p set = mem (p, []) set
      (* We want to find whether [p] is in [set], no matter what the
         associated path is (since the comparison function does not
         care about the path, we can a use the dummy []). *)
  end

  (* The path can use `Dismissed squares provided one is sure there is
     no wall. *)
  let nbh_accessible xy0 =
    let add_if_can_pass nbh (dir, xy) =
      if Labyrinth.wall_on xy0 dir = `False
        && Labyrinth.status xy <> `Non_explored then
        (dir, xy) :: nbh
      else nbh in
    List.fold_left add_if_can_pass [] (Labyrinth.Coord.nbh xy0)

  (** [path_to_closer pos cond] search for a square [p] satisfying
      [cond p] as close as possible to the square [pos] while staying
      in the square already visited.  If no such square [p] exists,
      return [[]].  Otherwise, return a list of directions to reach it. *)
  let rec path_to_closer pos cond =
    assert(Labyrinth.status pos <> `Cross_roads);
    let init = S.singleton (pos,[]) in
    search cond init init

  (** [search cond n v] construct an increasing sequance of neighbors
      until a square [p] satisfying [cond p] is found or there is no
      more squares to explore.  [n] is the set of squares that are
      reached from the initial position by a minimal path of k moves
      (where k is the number of times [search] was recursively called)
      and [v] are all previous squares reachable from the initial
      position in k moves or less.  *)
  and search cond n v =
    if S.is_empty n then []
    else
      let n_cond = S.filter cond n in
      if S.is_empty n_cond then begin
        let nbh_of_n (q,path) curr =
          let nbh_q ((n_curr,v_curr) as curr) (dir,pos) =
            if S.mem pos v_curr then curr
            else
              let newp = (pos, dir :: path) in
              (S.add newp n_curr, S.add newp v_curr) in
          List.fold_left nbh_q curr (nbh_accessible q)
        in
        let (n_new,v_new) = S.fold nbh_of_n n (S.empty, v) in
        search cond n_new v_new
      end
      else List.rev(snd(S.choose n_cond))

  let next_square_to_explore ()  =
    let pos = Labyrinth.robot_pos() in
    match Labyrinth.nbh_unexplored pos with
    | (_ :: _) as dirs ->
          (* Prefer the `Front position if it exists, otherwise choose
             randomly between left and right, and in the last case, go
             back. *)
          let l = List.map (fun (d,_) -> (Labyrinth.rel_dir d, d)) dirs in
          [ try List.assoc `Front l
            with Not_found ->
              try List.assoc `Left l
              with Not_found ->
                try List.assoc `Right l
                with Not_found ->
                  List.assoc `Back l
          ]                             (* 1 elmt list *)
    | [] ->
        let is_x_roads (sq,_) = Labyrinth.status sq = `Cross_roads in
        match path_to_closer pos is_x_roads with
        | [] -> no_exit_exists()
        | p -> p


  (** Explore the labyrinth
   ***********************************************************************)

  (* Use the battery level to choose the more appropriate values. *)
  let battery =
    let b = Mindstorm.battery_level conn in
    eprintf "Battery level: %i millivolts\n%!" b;
    b

  (** A wall is said to exists if the distance measured by the
      ultrasonic sencor is less than or equal to [wall_dist]. *)
  let wall_dist = 30

  (* FIXME: better react when the data is not available *)
  let is_crossing = function Some a -> a < 25 | None -> false
  let is_path = function Some a -> a <= 35 && a >= 25 | None -> false
  let is_floor = function Some a -> a > 35 | None -> false

  let r = Robot.make()

  let run_loop _ = Robot.run r

  let color = Robot.light conn light_port r
  let ultra = Robot.ultrasonic conn ultra_port r
  let touch  =
    Mindstorm.Sensor.set conn switch_port1 `Switch `Bool;
    Mindstorm.Sensor.set conn switch_port2 `Switch `Bool;
    let get port = (Mindstorm.Sensor.get conn port).Mindstorm.Sensor.scaled in
    Robot.meas r (fun () -> (get switch_port1) = 1 || (get switch_port2) = 1)

  let idle = Robot.meas r begin fun () ->
    let (state,_,_,_) = Motor.get conn motor_left in
    state.Motor.run_state = `Idle
  end
  let idle_ultra =
    Robot.meas r (fun () ->
                    let (state,_,_,_) = Motor.get conn motor_ultra in
                    state.Motor.run_state = `Idle)

  let speed motor ?tach_limit sp =
    Motor.set conn motor (Motor.speed ?tach_limit (-sp))

  (** The robot resets the position of its 'eyes'. *)
  let reset_ultra angle set_wall k =
    set_wall();
    Robot.event_is idle_ultra (fun _  -> k());
    speed motor_ultra ~tach_limit:(abs angle) (if angle >= 0 then 20 else -20)

  (** The robot turns its 'eyes' to look around. *)
  let see_ultra angle set_wall k =
    Robot.event_is idle_ultra (fun _ -> reset_ultra (-angle) set_wall k);
    speed motor_ultra ~tach_limit:(abs angle) (if angle >= 0 then 20 else -20)

  (** [is_wall()] returns true if there is a wall in front of the robot. *)
  let is_wall () =
    match Robot.read ultra with
    | Some v -> v <= wall_dist
    | None -> false (* FIXME: should leave the wall in an unknown state *)

  (** [set_wall dir] looks front and updates the wall in the direction [dir]. *)
  let set_wall dir =
    Labyrinth.set_wall dir (is_wall())

  let look_left k =
    see_ultra (-220) (fun _ -> set_wall `Left) (fun _ -> k())

  let look_right k =
    see_ultra 220 (fun _ -> set_wall `Right) (fun _ -> k())

  let look_front k =
    set_wall `Front;
    k()

  let look_back k =
    see_ultra (-420) (fun _ -> set_wall `Back) (fun _ -> k())

  let look_walls k =
    speed motor_left 0;
    speed motor_right 0;
    let wall d =
      Labyrinth.wall_on (Labyrinth.robot_pos()) (Labyrinth.abs_dir d) in
    if wall `Left = `Unknown then
      look_left (if wall `Right = `Unknown then
                   fun _ -> look_right (if wall `Front = `Unknown then
                                         fun _ -> look_front k else k)
                 else if wall `Front = `Unknown then
                   fun _ -> look_front k
                 else k)
    else if wall `Right = `Unknown then
      look_right (if wall `Front = `Unknown then (fun _ -> look_front k)
                  else k)
    else if wall `Front = `Unknown then
      look_front k
    else k()

  let sp_motor = 20

  (** The robot goes straight a little bit without testing if it is on a
      crossing or a path. *)
  let go_straight_before_do tl k =
    Robot.event_is idle (fun _ -> k());
    speed motor_left ~tach_limit:tl sp_motor;
    speed motor_right ~tach_limit:tl sp_motor

  (** The robot goes back a little bit without testing if it is on a
      crossing or a path. *)
  let go_back_before_do k =
    Robot.event_is idle (fun _ -> k());
    speed motor_left ~tach_limit:100 (-sp_motor);
    speed motor_right ~tach_limit:100 (-sp_motor)

  let go_straight_and_check k =
    Robot.event_is touch found_exit;
    go_straight_before_do 100 k

  (** Restarts the exploration of the labyrinth.
      (the exploration begins with a search of a new path, not with
      a look of the walls.) *)
  let rec restart_solve () =
    (*let l = next_square_to_explore() in
      let l =
        List.map (function `N -> "N" | `S -> "S" | `E -> "E" | `W -> "W") l in
      Printf.printf "[%s]\n%!" (String.concat "," l);*)
      follow_path (fun _ -> look_walls restart_solve) (next_square_to_explore())

  and go_next_square k =
    Robot.event_is touch found_exit;
    Robot.event color is_crossing (fun _ -> k());
    let sp = if Random.bool() then (sp_motor - 5) else -(sp_motor - 5) in
    Robot.event color is_floor (fun _ -> rectif k 20 sp);
    speed motor_left sp_motor;
    speed motor_right sp_motor

  (** The robot goes to the next square, i.e. the next crossing.
      But if it sees a wall, it restarts the exploration of the labyrinth. *)
  and wall_or_go_next_square k =
    if is_wall() then (
      Labyrinth.set_wall `Front true;
      Mindstorm.Sound.play C.conn "Woops.rso";
      go_back_before_do (fun _ -> restart_solve())
    )
    else (
      Labyrinth.move();
      go_straight_before_do 80 (fun _ -> go_next_square (fun _ -> k()))
    )

  (** The robot rectifies its trajectory to go back to the path. *)
  and rectif k tl sp =
    (* Robot.event_is touch found_exit;*)
    Robot.event color is_path (fun _ -> go_next_square k);
    Robot.event color is_crossing (fun _ -> k());
    Robot.event_is idle (fun _ -> rectif k (tl*2) (-sp));
    speed motor_left ~tach_limit:tl (-sp);
    speed motor_right ~tach_limit:tl sp

 (** The robot turns on itself. *)
  and turn_degree tl sp k =
    let turn tl sp k =
      (*Printf.printf "turn %i %i \n%!" tl sp;*)
      Robot.event_is idle k;
      speed motor_left ~tach_limit:tl (-sp);
      speed motor_right ~tach_limit:tl sp in
    go_straight_before_do 180 (fun _ -> turn tl sp (fun _ -> k()))

  and turn dir k =
    match dir with
    | `Left -> Labyrinth.turn `Left;
        turn_degree 180 sp_motor k
    | `Right -> Labyrinth.turn `Right;
        turn_degree 180 (-sp_motor) k
    | `Front -> Labyrinth.turn `Front;
        go_straight_and_check k
    | `Back -> Labyrinth.turn `Back;
        turn_degree 360 sp_motor k

  and go dir k =
      turn dir (fun _ -> wall_or_go_next_square (fun _ -> k()))

  and follow_path k path =
    Labyrinth.set_current_path path;
    match path with
    | [] -> k()
    | d :: tl -> go (Labyrinth.rel_dir d) (fun _ -> follow_path k tl)

end


(* Local Variables: *)
(* compile-command: "make -k solver.cmo" *)
(* End: *)
