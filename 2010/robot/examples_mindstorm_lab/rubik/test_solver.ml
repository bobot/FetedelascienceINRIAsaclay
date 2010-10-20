(* File: test_solver.ml

   Copyright (C) 2008

     Christophe Troestler <Christophe.Troestler@umh.ac.be>
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


open Rubik

let () =
  let moves = [F,3; R,2; U,1; B,3; D,1; L,2; R,3; U,2; R,3; B,1;
               F,3; F,1; R,1; U,3; B,1; D,2; L,3; B,2 ] in
  (* Patterns from http://www.math.ucf.edu/~reid/Rubik/patterns.html *)
  (* let moves = [F,2; B,2; R,2; L,2; U,2; D,2] in *)
  (* let moves = [U,3; L,3; U,3; F,3; R,2; B,3; R,1; F,1; U,1; B,2;
               U,1; B,3; L,1; U,3; F,1; U,1; R,1; F,3] in *)
  (*let moves = [R,2; L,3; D,1; F,2; R,3; D,3; R,3; L,1; U,3; D,1; R,1;
               D,1; B,2; R,3; U,1; D,2] in*)

  let moves = List.map (fun m -> Cubie.move (Move.make m)) moves in
  let cube = List.fold_left Cubie.mul Cubie.id moves in
  display_cube cube;


  let solution = Solver.find_first cube in
()
