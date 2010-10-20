(* File: labyrinth.ml

   Copyright (C) 2008

     Marc Ducobu <el_marcu@users.sourceforge.net>

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

type dir = [`N | `S | `E | `W]
type dir_rel = [`Left | `Front | `Right | `Back]
type wall  = [`True | `False | `Unknown]
type state = [`Explored | `Cross_roads | `Dismissed | `Non_explored]

module type T =
sig
  type dir = [`N | `S | `E | `W]
  type dir_rel = [`Left | `Front | `Right | `Back]

  module Coord :
  sig
    type t = int*int
    val compare : t -> t -> int
    val move :  t -> dir -> t
    val nbh : t -> (dir * t) list
  end

  val nbh_explored : Coord.t -> (dir * Coord.t) list
  val nbh_unexplored : Coord.t -> (dir * Coord.t) list
  val wall_on : Coord.t -> dir -> wall
  val status : Coord.t -> state

  val robot_pos : unit -> Coord.t
  val robot_dir : unit -> dir
  val rel_dir : dir -> dir_rel
  val abs_dir : dir_rel -> dir
  val set_wall : dir_rel -> bool -> unit
  val turn : dir_rel -> unit
  val move : ?affects:(Coord.t list -> Coord.t list -> unit) -> unit -> unit
end

(*************************************************************************
 *                           Implementation
 *************************************************************************)

type square = { mutable s_state: state;
                mutable wall_W: wall;
                mutable wall_N: wall; }

(* For the current realisation, it is enough but in general a more
   extensible datastructure is needed.  We have chosen this for
   simplicity. *)
let taille_lab = 10
let i0 = taille_lab
let j0 = taille_lab

(* Global state of this module: what we know of the labyrinth and the
   state of the robot. *)
let lab =
  let n = 2 * taille_lab + 1 in
  let make_square _ = { s_state = `Non_explored;
                        wall_W = `Unknown;
                        wall_N = `Unknown; } in
  Array.init n (fun _ -> Array.init n make_square)

let robot_pos = ref (0, 0)

let robot_orient = ref `N

let lab_coord (x,y) =
  let i = i0 + x and j = j0 + y in
  if 0 <= i && i < Array.length lab && 0 <= j && j < Array.length lab.(0)
  then (i,j)
  else invalid_arg "lab_coord: not in board"
;;

module Coord =
struct
  type t = int * int

  let compare (a, b) (c, d) =
    if (a < c) || ((a = c) && (b < d)) then -1
    else if (a = c) && (b = d) then 0
    else 1

  let move (x,y) = function
    | `N -> (x, y+1)
    | `S -> (x, y-1)
    | `W -> (x-1, y)
    | `E -> (x+1, y)

  (* The labyrinth is supposed to be potentially infinite so there is
     no constraint on the possible neighbors. *)
  let nbh xy =
    let c = move xy in [(`N, c `N); (`S, c `S); (`E, c `E); (`W, c `W)]
end


let wall_on xy d =
  let (i,j) = lab_coord xy in
  match d with
  | `N -> lab.(i).(j).wall_N
  | `S -> lab.(i).(j-1).wall_N
  | `W -> lab.(i).(j).wall_W
  | `E -> lab.(i+1).(j).wall_W

let status xy =
  let (i,j) = lab_coord xy in
  lab.(i).(j).s_state

let set_status xy v =
  let (i,j) = lab_coord xy in
  lab.(i).(j).s_state <- v

(* Attribute numbers (mod 4) to the directions in a clockwise fashion. *)
let int_of_dir = function `N -> 0 | `E -> 1 | `S -> 2 | `W -> 3
let int_of_dir_rel = function `Front -> 0 | `Right -> 1 | `Back -> 2 | `Left -> 3

let rel_dir (dir:dir) : dir_rel =
  match (int_of_dir dir - int_of_dir !robot_orient + 4) mod 4 with
  | 0 -> `Front
  | 1 -> `Right
  | 2 -> `Back
  | _ -> `Left

let abs_dir (dir:dir_rel) : dir =
  match (int_of_dir_rel dir + int_of_dir !robot_orient) mod 4 with
  | 0 -> `N
  | 1 -> `E
  | 2 -> `S
  | _ -> `W

let opposite dir = match dir with
  | `N -> `S
  | `S -> `N
  | `E -> `W
  | `W -> `E

let nbh_explored xy0 =
  let add_if_explored nbh (dir, xy) =
    if wall_on xy0 dir = `False && status xy <> `Non_explored
      && status xy <> `Dismissed then
      (dir, xy) :: nbh
    else nbh in
  List.fold_left add_if_explored [] (Coord.nbh xy0)

let nbh_unexplored xy0 =
  let add_if_unexplored nbh (dir, xy) =
    if wall_on xy0 dir <> `True && status xy = `Non_explored then
      (dir, xy) :: nbh
    else nbh in
  List.fold_left add_if_unexplored [] (Coord.nbh xy0)


(* Updating the state
 ***********************************************************************)

(* Initialisation: the current square is visited (not yet fully) *)
let () =
  set_status !robot_pos `Cross_roads

(** Return [true] if all accessible neighboor sqaure are explored. *)
let fully_explored xy0 =
  let explored (dir,xy) =
    wall_on xy0 dir = `True || status xy <> `Non_explored in
  List.fold_left (fun hist sq -> hist && explored sq) true (Coord.nbh xy0)

(** Update the status of the square [xy] according to the available
    information. *)
let update xy =
  match status xy with
  | `Explored | `Dismissed -> ()
  | `Cross_roads | `Non_explored ->
      if fully_explored xy then set_status xy `Explored

let set_wall (d:dir_rel) w =
  let (i,j) = lab_coord !robot_pos in
  let w = if w then `True else `False in
  begin match abs_dir d with
  | `N -> lab.(i).(j).wall_N <- w
  | `S -> lab.(i).(j-1).wall_N <- w
  | `W -> lab.(i).(j).wall_W <- w
  | `E -> lab.(i+1).(j).wall_W <- w
  end;
  (* Knowing a new wall will not improve the knowledge of neighbors. *)
  update !robot_pos


let turn d =
  robot_orient := abs_dir d
    (* No new information => state does not change *)
;;

module S = Set.Make(Coord)

let not_in_labyrinth xy =
  try ignore(lab_coord xy); false with _ -> true

exception Not_closed

(* Start a search from the square [xy0] and determine if it belongs to
   a zone surrounded by explored squares.  Do a depth first if the
   zone is not closed, that is quickly detected.  If the square [xy0]
   is visited, it is added to the [boundary] as it may need to be
   updated. *)
let dismiss_squares db0 xy0 =
  let rec search ((dismissed, boundary) as db) xy =
    if not_in_labyrinth xy then raise Not_closed
    else match status xy with
    | `Explored | `Cross_roads -> (dismissed, S.add xy boundary)
    | `Dismissed (* should not happen *) | `Non_explored ->
        if S.mem xy dismissed then db   (* already in zone *)
        else
          let db' = (S.add xy dismissed, boundary) in
          List.fold_left (fun db' (_,xy') -> search db' xy') db' (Coord.nbh xy)
  in
  try search db0 xy0 with Not_closed -> db0

let move ?affects () =
  robot_pos := Coord.move !robot_pos !robot_orient;
  set_status !robot_pos `Cross_roads;
  (* The knowledge that the current square is visited may change its
     left and right neighbors (and maybe more). *)
  let dismissed, boundary =
    List.fold_left (fun db (_,xy) ->
                      dismiss_squares db xy
                   ) (S.empty, S.empty) (Coord.nbh !robot_pos) in
  S.iter (fun xy -> set_status xy `Dismissed) dismissed;
  (* There may be `Cross_roads around the zone whose only exit points
     inside the zone and which are no longer worth exploring *)
  S.iter (fun xy -> update xy) boundary;
  update !robot_pos; (* maybe more info exists now that can improve the
                        status of the current square. *)
  match affects with
  | None -> ()
  | Some f -> f (S.elements dismissed) (S.elements boundary)


(* Accessors
 ***********************************************************************)

let robot_pos () = !robot_pos

let robot_dir () = !robot_orient


(* Local Variables: *)
(* compile-command: "make -k labyrinth.cmo" *)
(* End: *)
