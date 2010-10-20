(* File: movement.ml

   Copyright (C) 2008

     Dany Maslowski <dan_86@users.sourceforge.net>

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
open Random

(** We parametrise the solver by the connections to the ports (so as to
    change them easily) *)
module Make(C: sig
              val conn : Mindstorm.bluetooth Mindstorm.conn
              val motor_fighter : Motor.port
              val motor_hand : Motor.port
              val motor_pf : Motor.port
              val push_hand_port : Sensor.port
              val push_fighter_port : Sensor.port
              val cog_is_set_left : bool
            end) =

struct
  open C
  open Printf

  type cont = unit -> unit

  let r = Robot.make()

  let execute() = Robot.run r

  let end_cont () = Robot.stop r

  (* State of the rubik robot *)

  let cube_is_held = ref true
    (** Indicates whether the cube is is not held by the "hand". *)

  let cog_is_left = ref (C.cog_is_set_left)
    (** To know if cogs are placed to turn left or right.  This
        variable is used because there is a large space between the
        teeth of cogs (we lose about 30 degrees when we change the
        direction of rotations of the platform). *)

  let speed motor ?tach_limit sp =
    Motor.set conn motor (Motor.speed ?tach_limit (sp))

  let get_tc_pf () = let (x,_,_,_) = Motor.get C.conn motor_pf in x

  let motor_is motor state =
    Robot.meas r (fun () ->
                    let (st,_,_,_) = Motor.get C.conn motor in
                    st.Motor.run_state = state)

  let idle_hand    = motor_is motor_hand `Idle
  let idle_fighter = motor_is motor_fighter `Idle
  let idle_pf      = motor_is motor_pf `Idle
  let running_pf   = motor_is motor_pf `Running

  let hand_push = Robot.touch C.conn push_hand_port r
  let fighter_push = Robot.touch C.conn push_fighter_port r

  (** Give an appropriate position to the cogs to turn left if the
      boolean bool is true or to turn rigth otherwise. *)
  let set_cog turn_left k =
    if turn_left then
      if !cog_is_left then k()
      else (Robot.event_is idle_pf k;
            cog_is_left := true ;
            speed motor_pf ~tach_limit:38 (-3))
    else
      if !cog_is_left then (Robot.event_is idle_pf k;
                            cog_is_left := false;
                            speed motor_pf ~tach_limit:38 3)
      else k()


  let hold_rubik k =
    if !cube_is_held then k()
    else (Robot.event_is idle_hand k;
          cube_is_held := true;
          speed motor_hand ~tach_limit:110 (-30))

  let free_rubik k =
    if !cube_is_held then begin
      Robot.event_is hand_push (fun _ -> speed motor_hand 0;  k());
      cube_is_held := false;
      speed motor_hand 15
    end
    else k()

  let usleep s = ignore(Unix.select [] [] [] s)

  let reset_fighter k =
    Robot.event_is fighter_push (fun _ -> speed motor_fighter 0; k());
    speed motor_fighter (-5)

  (** Reset the position of the fighter after having kicked the cube *)
  let kick_back k =
    Robot.event_is fighter_push begin fun _ ->
      speed motor_fighter 0;
      usleep 0.3; (* let the bump happen before measuring again *)
      reset_fighter k
    end;
    speed motor_fighter (-25)

  let kick k =
    hold_rubik begin fun _ ->
      Robot.event_is idle_fighter (fun _ -> kick_back k);
      speed motor_fighter ~tach_limit:80 100
    end

  (** Turn the platform slowly to adjust it with precision *)
  let turn_pf_slowly tach_limit v k =
    Robot.event_is idle_pf (fun _ -> speed motor_pf 0; k());
    speed motor_pf ~tach_limit v

  (* How many degrees the motor must rotate for the cube platform to
     make the fourth of a turn.  *)
  let degree_per_quarter = 300

  let turn_pf qt k =
    let turn () = free_rubik begin fun _ ->
      let tl17 = qt*(-331) in
      if qt > 0 then (
        Robot.event_is idle_pf (fun _ -> turn_pf_slowly (-tl17) (-8) k);
        speed motor_pf ~tach_limit:(qt * degree_per_quarter) (-100))
      else (
        Robot.event_is idle_pf (fun _ -> turn_pf_slowly tl17 8 k);
        speed motor_pf ~tach_limit:(-qt * degree_per_quarter) 90)
    end in
    set_cog (qt > 0) turn

  (** Rectify the platform after having turned the cube because there is a
      small error caused by a space between the cube and the "hand". *)
  let rectif_pf turn_left tl10 v10 k=
    set_cog (not turn_left) begin fun _ ->
      Robot.event_is idle_pf k;
      speed motor_pf ~tach_limit:tl10 v10
    end

  (** Turn the platform slowly (the cube is held) to have a good precision *)
  let turn_rubik_slowly turn_left tl30 tl10 v30 v10 k =
    set_cog turn_left begin fun _ ->
      Robot.event_is idle_pf (fun _ -> rectif_pf turn_left tl10 v10 k);
      speed motor_pf ~tach_limit:tl30 v30
    end

  (** Turn the platform (the cube is held) *)
  let turn_rubik turn_left tach_limit tl30 tl10 v100 v30 v10 k =
    hold_rubik begin fun _ ->
      Robot.event_is idle_pf
        (fun _ -> turn_rubik_slowly turn_left tl30 tl10 v30 v10 k);
      speed motor_pf ~tach_limit v100
    end

  let turn_rubik_left ~cont =
    set_cog true (fun _ -> turn_rubik true 450 249 68 (-70) (-20) (10) cont)
  let turn_rubik_right ~cont =
    set_cog false (fun _ -> turn_rubik false 450 225 39 (70) 20 (-10) cont)
      (* Last values : turn_rubik false 450 235 54 (70) 20 (-10) cont *)
  let turn_rubik_half ~cont =
    let left = Random.bool() in
    set_cog left (fun _ ->
                    if left then
                      turn_rubik true 900 426 64 (-70) (-20) (10) cont
                    else
                      turn_rubik false 900 419 57 70 20 (-10) cont
                 )

  let () =
    kick_back (fun _ -> free_rubik (end_cont));
    execute()

end


(* Local Variables: *)
(* compile-command: "make -k movement.cmo" *)
(* End: *)
