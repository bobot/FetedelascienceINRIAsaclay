(* File: a_star.ml

   Copyright (C) 2008

     Julie de Pril <julie_87@users.sourceforge.net>

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


(** Functor using a [Phase] module to develop some strategies to find a
    sequence of moves to get into a goal state. *)
module Make(P:
  sig
    include Rubik.Coordinate
    val max_moves : int
    val in_goal : t -> bool
  end) =
struct

  open Rubik

  module PQ = Priority_queue

  let mul = P.initialize_mul()
  let pruning = P.initialize_pruning()

  (** [search_seq_to_goal p in_goal] returns a list of moves that lead
      to the a permutation [q] (such that [in_goal q] is true) from
      the permutation [p]. *)
  let search_seq_to_goal first max_moves =
    (* [get_children p] returns the "children" of the permutation [p] (ie all
       the permutations we can reach by applying a single move to [p]) with
       the associated move. *)
    let get_children p = List.map (fun move -> (mul p move, move)) P.Move.all in
    (* [aStar] recursively searches a sequence of moves to get into the goal
       state. (A* algorithm) *)
    let rec aStar opened =
      (* [opened] is the set of all pending candidates. *)
      if PQ.is_empty opened (* No more candidates. *) then []
      else
        let (p,seq,pcost) = PQ.take opened in
        if P.in_goal p (* Get into the goal state! *) then [List.rev seq]
        else begin
          let children = get_children p in
          (* We update the set [opened] with the children of [p]. *)
          let opened =
            List.fold_left begin fun opened (child,m) ->
              let fchild = pcost+1 + pruning child in
              if fchild <= max_moves then (* The path is not too long. *)
                let c = (child, m::seq, pcost+1) in
                PQ.add fchild c opened;
                opened
              else opened
            end
              opened children
          in
          aStar opened
        end
    in
    let start = (first,[],0) in
    let opened = PQ.make (max_moves+1) in
    PQ.add (pruning first) start opened;
    aStar opened
end





