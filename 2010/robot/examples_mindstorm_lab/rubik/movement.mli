(* File: movement.mli

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


module Make(C: sig
              val conn : Mindstorm.bluetooth Mindstorm.conn
                (** The bluetooth connection of the robot. *)
              val motor_fighter : Mindstorm.Motor.port
                (** The port of the fighther motor. *)
              val motor_hand : Mindstorm.Motor.port
                (** The port of the hand motor. *)
              val motor_pf : Mindstorm.Motor.port
                (** The port of the platform motor. *)
              val push_hand_port : Mindstorm.Sensor.port
                (** The port of the hand switch. *)
              val push_fighter_port : Mindstorm.Sensor.port
                (** The port of the fighter switch. *)
              val cog_is_set_left : bool
                (** True if cogs are placed to turn platform in clockwise
                    False if cogs are placed to turn plaftform in clockwis. *)
            end):
sig
  type cont = unit -> unit
    (** Type of continuations.  Many functions below take a parameter
        of this type.  The continuation is executed after the current
        action is finished. *)

  val execute : unit -> unit
    (** Launch the robot to do a task. *)

  val end_cont : unit -> unit
    (** Stop the Robot momentary after the end of one task*)

  val kick : cont -> unit
    (** Kick the rubik's cube to turn it by 90 degrees. *)

  val turn_pf : int -> cont -> unit
    (** [turn_pf t k] turn the platform of [t] quarters of turn (if
        [t] is negative, it turn in the other way round. *)

  val turn_rubik_left : cont:cont -> unit
    (** Turn the bottom row of the cube by 90 degrees clockwise. *)

  val turn_rubik_right : cont:cont -> unit
    (** Tx2urn the bottom row of the cube by 90 degrees counter clockwise. *)

  val turn_rubik_half : cont:cont -> unit
    (** Turn the bottom row of the cube by half a turn. *)

  val free_rubik : cont -> unit
    (** Move the hand to free the cube *)

  val hold_rubik : cont -> unit
    (** Move the hand to hold the cube *)
end
