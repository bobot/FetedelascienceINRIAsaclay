open Graphics
open Rubik

(** Functions to draw rubik cubes on a [Graphics] window. *)

type color6 = color * color * color * color * color * color
    (** The colors of the 6 faces of a cube given in the following
        order of the faces U, L, F, R, B, D. *)

val cube : ?pen:int -> ?angle:int -> int -> int -> color6 -> int
  -> (Cubie.t -> unit)
  (** [cube x0 y0 initial_cubie_color lgth_sq angle] returns a
      function which, given a cube, an array of corners and an array
      of edges, prints on the screen the edges and the corners of the
      cubi.  [lght_sq] is the length of the squares.  [(x0, y0)] gives
      the position of bottom left corner of the (smaller) rectangle
      surrounding the cube image.

      @param angle the angle, in degrees, of the upper and right faces
      with the horizontal.  Default: [45].  *)
