(* File: translator.mli

   Copyright (C) 2008

     Dany Maslowski <dan_86@users.sourceforge.net>

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

module type TranslatorT =
sig

  val show_cube : unit -> unit
    (** [show_cube()] opens the "hand" to free the cube so that
        everybody can see it. *)

  val make : Rubik.generator * int -> unit
    (** [make m] drives the robot to execute the physical movement
        associated to the the move [m].  *)

  val face_iter : (Rubik.generator -> int -> unit) -> unit
    (** [face_iter f] puts successively each face of the cube on top
        and run [f] (e.g. to take a snapshot).  [f g i] receives the
        face [g], which is in the position [i] (see the documentation
        of {!Init_color} for its meaning).  Here we choose the letters
        associated to faces (see the documentation of the fonction
        [transform_gen]) so that the initial state is correct after
        all snapshot.=?? *)

  val return_face_init : unit -> unit
    (** [return_face_init] is the inverse of [face_iter]. [return_face_init]
        called after a call of [face_iter] puts the cube in its initial 
        position. *)

end

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
                    False if cogs are placed to turn plaftform in clockwise. *)
            end): TranslatorT
