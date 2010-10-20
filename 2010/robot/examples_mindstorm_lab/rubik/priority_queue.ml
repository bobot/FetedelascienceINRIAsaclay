(* File: priority_queue.ml

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


type 'a t = {
  pq : 'a Queue.t array;
  mutable p : int; (* index of the first non-empty queue or [length
                      pq] if empty.  (This is to lower the cost of
                      repeatedly searching for this index.)*)
}

exception Empty

let min i j = if (i:int) <= j then i else j

let make n = { pq = Array.init n (fun _ -> Queue.create());
               p = n }

let add priority v t =
  if priority < 0 || priority >= Array.length t.pq then
    invalid_arg "priority_queue.add: priority out of bounds";
  Queue.add v t.pq.(priority);
  t.p <- min t.p priority

let push = add

let rec move_to_nonempty t =
  if t.p = Array.length t.pq then raise Empty;
  if Queue.is_empty t.pq.(t.p) then begin
    t.p <- t.p + 1;
    move_to_nonempty t
  end

let rec take t =
  move_to_nonempty t;                   (* or raise Empty *)
  Queue.take t.pq.(t.p)

let pop = take

let peek t =
  move_to_nonempty t;                   (* or raise Empty *)
  Queue.peek t.pq.(t.p)

let top = peek

let is_empty t =
  try move_to_nonempty t; false with Empty -> true


(* Expected to be seldom used => do not charge for it at every insertion *)
let length t =
  Array.fold_left (fun s p -> s + Queue.length p) 0 t.pq


(* Local Variables: *)
(* compile-command: "make -k priority_queue.cmo" *)
(* End: *)
