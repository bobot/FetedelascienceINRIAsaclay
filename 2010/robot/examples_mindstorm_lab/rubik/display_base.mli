(* File: display_base.mli

   Copyright (C) 2008

     Christophe Troestler <Christophe.Troestler@umh.ac.be>
     WWW: http://math.umh.ac.be/an/software/

     Marc Ducobu <el_marcu@users.sourceforge.net>

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

open Rubik

type color = int
    (** RGB color compatible with the [Graphic] module. *)

(** The basic colors of the Rubik's cube *)
val white : color
val yellow : color
val green : color
val blue : color
val orange : color
val red : color
val black : color


type geom = {
  xy0 : float * float;
  (** Coordinates of the bottom left corner of smaller rectangle
      surrounding the cube figure. *)
  width : float;
  (** The width of a (non slanted) facelet *)
  height : float;
  (** The height of a (non slanted) facelet *)
  angle : int;
  (** Angle, in degrees, of the right and up faces *)
}

(** The colors of the 6 faces and of the lines.  *)
type colors = {
  color_F : color;
  color_B : color;
  color_L : color;
  color_R : color;
  color_U : color;
  color_D : color;
  color_lines : color;
}


val geom : geom
  (** Default values for the cube representation. *)

val cube : ?geom:geom -> ?colors:colors -> Cubie.t -> unit
  (** Draw the cube on the [Graphics] window. *)

val cube_tikz : out_channel -> ?geom:geom -> ?colors:colors -> Cubie.t -> unit
  (** [cube_tikz fh c] writes pgf commands to represent the cube [c]
      on the channel [fh]. *)
