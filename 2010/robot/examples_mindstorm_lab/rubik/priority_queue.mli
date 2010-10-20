(* File: priority_queue.mli

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

type 'a t
  (** Simple mutable priority queue, the priorities being integers and
      the values stored being of type ['a]. *)

exception Empty
  (** Exception indicating that the queue is empty. *)

val make : int -> 'a t
  (** [make n] returns a priority queue for priorities ranging from
      [0] to [n - 1].  It is initially empty. *)

val add : int -> 'a -> 'a t -> unit
  (** [add priority v pq] adds the element [v] to the priority queue
      [pq] with the given [priority].  If [v] is already present in
      the queue, it is not affected by the insertion of the new
      element. *)

val push : int -> 'a -> 'a t -> unit
  (** [push] is a synonym for [add]. *)

val take : 'a t -> 'a
  (** [take pq] removes and returns the element in priority queue [pq]
      with the lower priority, or raises [Empty] if the queue is
      empty. *)

val pop : 'a t -> 'a
  (** [pop] is a synonym for [take]. *)

val peek : 'a t -> 'a
  (** [peek pq] returns the element in queue [pq] with the least,
      without removing it from the queue, or raises [Empty] if the
      queue is empty. *)

val top : 'a t -> 'a
  (** [top] is a synonym for [peek]. *)

val is_empty : 'a t -> bool
  (** [is_empty pq] tells whether the priority queue [pq] is empty or
      not. *)

val length : 'a t -> int
  (** Return the number of elements in a queue. *)
