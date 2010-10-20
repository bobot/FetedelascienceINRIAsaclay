(* File: solver.mli

   Copyright (C) 2008

     Christophe Troestler <chris_77@users.sourceforge.net>
     WWW: http://math.umh.ac.be/an/software/

     Julie de Pril <julie_87@users.sourceforge.net>

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(** Strategies to solve the cube. *)

open Rubik

val find_first : Cubie.t -> (generator * int) list
  (** Returns the first solution it finds, even if it is
      suboptimal. *)
