(* File: labyrinth.mli

   Copyright (C) 2008

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


(** Current knowledge of the labyrinth and of the robot position.
    This module has hiddden state for simplicity. *)

(** The signature of a "Labyrinth" module.  This module posseses an
    inner state that knows the position and orientation of the robot
    as well as the currently known part of the labyrinth. *)
module type T =
sig
  type dir = [`N | `S | `E | `W]
      (** Absolute directions, i.e. directions fixed w.r.t. the labyrinth. *)
  type dir_rel = [`Left | `Front | `Right | `Back]
      (** Relative directions, i.e. directions relatiove to the robot
          orientation. *)

  (** Coordinates of the squares of the labyrinth. *)
  module Coord :
  sig
    type t = int * int
        (** Position [(x,y)] of the robot relative to its initial
            position.  (The coordinates of the initial position thus
            being [(0,0)]).  *)

    val compare : t -> t -> int
      (** Lexicographic comparison function. *)

    val move : t -> dir -> t
      (** [move pos dir] returns the coordinates the the position
          reached by starting from [pos] and executing a move in the
          direction [dir].  *)

    val nbh : t -> (dir * t) list
      (** [Coord.nbh xy] return absolute directions (relative to [xy])
          and coordinates of all neighbors of [xy]. *)
  end

  val nbh_explored : Coord.t -> (dir * Coord.t) list
    (** [nbh_explored xy] returns a list of accessible neighbors that
        the robot has already visited (i.e. [`Explored] or
        [`Cross_roads]). *)
  val nbh_unexplored : Coord.t -> (dir * Coord.t) list
    (** [nbh_explored xy] returns the list of neighbors which are not
        known to be are accessible or not (one does not know whether
        there is a all) or that are known to have never been visited
        by the robot. *)
  val wall_on : Coord.t -> dir -> [`True | `False | `Unknown]
    (** [wall_on xy d] tells whether there is a wall in the direction
        [d] on the square [xy]. *)
  val status : Coord.t -> [`Explored | `Cross_roads
                          | `Dismissed | `Non_explored]
    (** [status xy] tells whether the square [xy]
        - is fully [`Explored], meaning all its accessible neighbors
        (i.e. without wall in between) are explored;
        - is [`Cross_roads] meaning the robot has been there but one
        of its accessible neighbors that has not yet been explored;
        - is [`Dismissed] meaning that the system determined that
          the square is not worth exploring (because, for example,
          it is surrounded by explored squares);
        - [`Non_explored] meaning the robot has not been there. *)

  val robot_pos : unit -> Coord.t
    (** [robot_pos()] returns the current position [(x,y)] of the
        robot relative to its initial position. *)
  val robot_dir : unit -> dir
    (** [robot_dir()] returns the current orientation of the front of
        the robot.  *)
  val rel_dir : dir -> dir_rel
    (** Converts an absolute direction into a relative one. *)
  val abs_dir : dir_rel -> dir
    (** Converts a relative direction into an absolute one. *)


  val set_wall : dir_rel -> bool -> unit
    (** [set_wall d] store in the labyrinth whether or not there is a
        wall in the direction [d] of the current position of the
        robot.  *)

  val turn : dir_rel -> unit
    (** [turn d] turns the robot in the direction [d]. *)

  val move : ?affects:(Coord.t list -> Coord.t list -> unit) -> unit -> unit
    (** [move d] update the status of the robot given that it executes
        a move one square ahead of him.

        @param affects A move may have consequances on other squares,
        dismissing some and changing crossroads in explored.  This
        argument is a function that receives first new dismissed
        squares and second potentially updated squares and is executed
        after the move (and updates) is complete. *)
end

include T
