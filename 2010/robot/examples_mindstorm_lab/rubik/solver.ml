(* File: solver.ml

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

open Printf
open Rubik

(* module Search = A_star *)
module Search = Ida
module Solver1 = Search.Make(Phase1)
module Solver2 = Search.Make(Phase2)

(********* Helpers *********)
let move1 = Cubie.move
let move2 = Phase2.Move.move


(********* Test *********)
let find_first cube =

  (********* Phase 1 *********)
  let cubeP1 = Phase1.of_cube cube in

  printf "Sequence phase 1:\n%!";
  let seq1 = Solver1.search_seq_to_goal cubeP1 Phase1.max_moves in
  List.iter (fun sol -> printf "%s\n" (Phase1.Move.to_string sol)) seq1;

  (********* Phase 2 *********)
  let apply1 sol = List.fold_left (fun c m -> Cubie.mul c (move1 m)) cube sol in
  let cubesP2 = List.map (fun s -> (Phase2.of_cube(apply1 s)), s) seq1 in

  printf "Sequence phase 2:\n%!";
  (* let seq2 = Solver2.search_seq_to_goal cubeP2 Phase2.max_moves in *)
  let seq2 = Solver2.multiple_search (List.map fst cubesP2) Phase2.max_moves in
  List.iter (fun (init,sol) -> printf "%s\n" (Phase2.Move.to_string sol)) seq2;

  let solutions =
    List.map (fun (init, sol) -> (List.assoc init cubesP2, sol)) seq2 in

  let apply2 cube sol =
    List.fold_left (fun c m -> Cubie.mul c (move2 m)) cube sol in

  List.iter begin fun (s1,s2) ->
    let goal = apply2 (apply1 s1) s2 in
    printf "%s | %s => %s\n%!"
      (Phase1.Move.to_string s1) (Phase2.Move.to_string s2)
      (if Cubie.is_identity goal then "OK" else "KO");
    if not(Cubie.is_identity goal) then (
      (* display_cube goal; *)
      (* ignore(wait_next_event [Button_down]) *)
    );
  end solutions;

  match solutions with
  | [] -> []
  | (sol1, sol2) :: _ ->
      List.map Phase1.Move.generator sol1
      @ List.map Phase2.Move.generator sol2
;;


(*
let _ =
  (* Restart *)
  let nmoves = 12 in
  let seq1' = Solver1.solutions cubeP1 nmoves in
  printf "Phase 1 with %i moves: %i sols\n" nmoves (List.length seq1');

  let cubesP2 = List.map (fun s -> (Phase2.of_cube(apply1 s)), s) seq1' in
  let seq2 = Solver2.multiple_search (List.map fst cubesP2) Phase2.max_moves in
  let solutions =
    List.map (fun (init, sol) -> (List.assoc init cubesP2, sol)) seq2 in
  List.iter begin fun (s1,s2) ->
    let goal = apply2 (apply1 s1) s2 in
    printf "%s | %s => %s\n%!"
      (Phase1.Move.to_string s1) (Phase2.Move.to_string s2)
      (if Cubie.is_identity goal then "OK" else "KO");
    if not(Cubie.is_identity goal) then (
      display_cube goal;
      ignore(wait_next_event [Button_down])
    );
  end solutions;
*)
