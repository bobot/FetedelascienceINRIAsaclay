(* File: solver.mli

   Copyright (C) 2008

     Julie de Pril <julie_87@users.sourceforge.net>

     Christophe Troestler <Christophe.Troestler@umh.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


(** Functor using a [Labyrinth] module to develop some strategies to
    find the exit of a labyrinth.  *)
module Make(C: sig
              val conn : Mindstorm.bluetooth Mindstorm.conn
                (** The bluetooth connection of the robot. *)
              val motor_left : Mindstorm.Motor.port
                (** The port of the left wheel motor of the robot. *)
              val motor_right : Mindstorm.Motor.port
                (** The port of the right wheel motor of the robot. *)
              val motor_ultra : Mindstorm.Motor.port
                (** The port of the "head" motor of the robot, moving
                    the ultrasonic sensor. *)
              val light_port : Mindstorm.Sensor.port
                (** The port of the light sensor (directed to the
                    floor). *)
              val ultra_port : Mindstorm.Sensor.port
                (** The ultrasonic "head" port. *)
              val switch_port1 : Mindstorm.Sensor.port
                (** The port of the switch used to indicate that the
                    goal square has been reached. *)
              val switch_port2 : Mindstorm.Sensor.port
                (** The port of the switch used to indicate that the
                    goal square has been reached. *)
              module Labyrinth : Display.T
                (** The labyrinth structure with some additional
                    graphical display capabilities. *)
            end) :
sig

  val run_loop : unit -> unit
    (** Launch the robot (must be used last, after a strategy is
        constructed using the functions below.).  *)

  val stop : ?music:string -> unit -> unit
    (** Stop the event loop -- presumably because the exit was found
        or it was determined that there is no way out. *)

  val next_square_to_explore : unit -> Labyrinth.dir list
    (** [next_case_to_explore()] returns a list of directions leading
        to the next square to explore. *)

  type cont = unit -> unit
    (** Continuations taken by the fonctions. *)

  val look_back : cont -> unit
    (** [look_back k] makes the robot look behind it,
        and then continues with [k]. *)

  val look_walls : cont  -> unit
    (** [look_walls k] makes the robot look around in the 3 directions,
        and then continues with [k]. *)

  val follow_path: cont -> Labyrinth.dir list -> unit
    (** [follow_path k p] makes the robot follow the path [p], and then
        continues with [k]. *)

end
