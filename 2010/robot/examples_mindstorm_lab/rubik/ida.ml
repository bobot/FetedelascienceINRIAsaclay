(* File: ida.ml

   Copyright (C) 2008

     Christophe Troestler <chris_77@users.sourceforge.net>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(** Iterative deepening A* algorithm. *)


module Make(C :
  sig
    include Rubik.Coordinate
    val max_moves : int
    val in_goal : t -> bool
  end) =
struct

  let dir = Some(Filename.concat Filename.temp_dir_name "rubik-ocaml")
  (* let dir = None *)
  let mul = C.initialize_mul ?dir ()
  let pruning = C.initialize_pruning ?dir ()

  (* if [m'] commute with [m] we require that the "smaller" one be
     first (also exclude that [m'] and [m] have the same generator). *)
  let move_cannot_follow m m' =
    C.Move.commute m m' && C.Move.compare_gen m m' >= 0

  (* Return a list of possible paths (added to the already existing
     solutions [sols]). *)
  let rec add_solution sols cost_max  perm last_move path cost_path =
    (* We only return solutions at the max depth.  This is in case we
       have solutions at depth d at relaunch ar depth d+1: we then
       only get solutions of depth d+1. *)
    if C.in_goal perm && cost_path = cost_max
    then List.rev path :: sols (* sol found *)
    else begin
      let cost_path = cost_path + 1 in
      List.fold_left begin fun sols m ->
        if move_cannot_follow last_move m then sols (* skip move *)
        else
          let perm = mul perm m in
          let cost = cost_path + pruning perm in
          if cost > cost_max then sols  (* prune *)
          else add_solution sols cost_max  perm m (m :: path) cost_path
      end sols C.Move.all
    end

  let solutions init cost_max =
    List.fold_left (fun sols m ->
                      add_solution sols cost_max (mul init m) m [m] 1
                   ) [] C.Move.all

  let rec search_cost init cost cost_max =
    let sols = solutions init cost in
    Printf.eprintf "cost=%i #sols=%i\n%!" cost (List.length sols);
    if sols = [] && cost < cost_max then
      search_cost init (cost + 1) cost_max
    else sols

  let search_seq_to_goal init cost_max =
    if C.in_goal init then [[]]
    else search_cost init 1 cost_max


  let rec multiple_search_cost inits cost cost_max =
    let sols =
      List.fold_left begin fun sols init ->
        let sols_init = solutions init cost in
        List.fold_left (fun l s -> (init, s) :: l) sols sols_init
      end [] inits in
    Printf.eprintf "cost=%i #sols=%i\n%!" cost (List.length sols);
    if sols = [] && cost < cost_max then
      multiple_search_cost inits (cost + 1) cost_max
    else sols

  (* Search from multiple starting points.  Return the starting point
     and the path. *)
  let multiple_search inits cost_max =
    let sols = List.map (fun i -> (i,[])) (List.find_all C.in_goal inits) in
    if sols = [] then multiple_search_cost inits 1 cost_max
    else sols
end




(* Local Variables: *)
(* compile-command: "make -k ida.cmo" *)
(* End: *)
