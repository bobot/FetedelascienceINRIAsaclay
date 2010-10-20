(* File: run.ml

   Copyright (C) 2008

     Julie de Pril <julie_87@users.sourceforge.net>
     Dany Maslowski <dan_86@users.sourceforge.net>
     Marc Ducobu <el_marcu@users.sourceforge.net>

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

open Printf
open Rubik
open Graphics
module D = Display_base

module Motor = Mindstorm.Motor

let conn =
  let bt =
    if Array.length Sys.argv < 2 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);
      exit 1;
    )
    else Sys.argv.(1) in Mindstorm.connect_bluetooth bt

module Brick =
struct
  let conn = conn
  let motor_fighter = Motor.a
  let motor_hand = Motor.b
  let motor_pf = Motor.c
  let push_hand_port = `S2
  let push_fighter_port = `S1
  let cog_is_set_left = true
end

module M = Translator.Make(Brick)


let display_moves x y m =
  let m = List.map (fun (g,i) -> sprintf "%c%i" (char_of_generator g) i) m in
  moveto x y;
  draw_string (String.concat " " m)

let () =
  open_graph " 700x500+100+30";
  (********* Initialization of the cubie *********)
  let cubie, colors = Init_color.create_rubik M.face_iter M.return_face_init in

  (********* Graphical part *********)
  let geom = { D.geom with D.xy0 = (10.,30.) } in
  let display c = D.cube ~geom ~colors c in

  clear_graph();
  display cubie;
  ignore(wait_next_event [Key_pressed]);

  (********* Resolution part *********)
  let solution = Solver.find_first cubie in

  (* Print the solution in gray so on sees the evolution of moves *)
  set_color (rgb 180 180 180);
  display_moves 10 10 solution;

  let print_and_do (c, moves) m =
    let status = wait_next_event[Poll; Button_down] in
    if status.button then
      ignore(wait_next_event[Button_down]);
    let c_next = Cubie.mul c (Cubie.move (Move.make m)) in
    let moves = m :: moves in
    display c_next;
    display_moves 10 10 (List.rev moves);
    M.make m;
    (c_next, moves)
  in

  let cubie, _ = List.fold_left print_and_do (cubie, []) solution in
  M.show_cube();

  display cubie;
  ignore(wait_next_event [Key_pressed])
