(* File: ppm.mli

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

(** Simple PPM format reader. *)


type color = int
    (** Color, compatible with the [Graphics] module. *)

val as_matrix_exn : ?gamma:float -> ?bgr:bool -> string -> color array array
  (** [as_matrix_exn fname] reads the PPM file named [fname] and
      return the image in a form compatible with the [Graphics]
      library.  In particular, this means that the row of coordinate
      [0] is the top one.

      @param gamma the gamma correction.  The format specifies 2.2 but
      the default is [1.] because files seem to conform this.

      @param bgr return the image with red and blue color permuted.
      It is useful as images returned by webcams sometimes need this.
      Default: [false].

      @raise Failure if the file does not respect the PPM
      specification of the image is too big. *)
