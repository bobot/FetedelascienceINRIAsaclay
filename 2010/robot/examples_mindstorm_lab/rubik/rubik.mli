(* File: rubik.mli

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

(** Rubik cube group encodings. *)


(** {2 Rubik group generators} *)

(** Standard Rubik group generators. *)
type generator =
  | F     (** 90° CW turn of the {i front} face *)
  | B     (** 90° CW turn of the {i back} face *)
  | L     (** 90° CW turn of the {i left} face *)
  | R     (** 90° CW turn of the {i right} face *)
  | U     (** 90° CW turn of the {i up} face *)
  | D     (** 90° CW turn of the {i down} face *)

val char_of_generator : generator -> char
  (** Returns the generator as a character. *)

val generator_iter : (generator -> unit) -> unit
  (** [generator_iter f] iterates [f] on all the generators. *)


(** Abstract vision of moves. *)
module type MoveT =
sig
  type t
    (** The basic moves that the generators allow: F, F^2, F^3, B,
        B^2, B^3, L, L^2, L^3,...  Remember that X^4 = 1 for all
        generators X. *)

  val make : generator * int -> t
    (** [move (g, i)] returns the move corresponding to [g^i].
        @raise Invalid_Argument if [i < 1] or [i > 3]. *)

  val generator : t -> generator * int
    (** [generator] is the inverse function of {!Rubik.MoveT.make}. *)

  val all : t list
    (** [all] returns the array of all possible moves. *)

  val commute : t -> t -> bool
    (** [commutes m m'] tells whether the two moves [m] and [m']
        commutes (i.e. [m] * [m'] = [m'] * [m]).  (This is obviously
        the case when [have_same_gen m m'] is [true].) *)

  val compare_gen : t -> t -> int
    (** [compare_gen m n] returns a number [< 0] (resp. [= 0],
        resp. [ > 0]) if the generator of [m] is less than
        (resp. equal to, resp. greater than) the generator of [n].  It
        defines a total order.  It is compatible with [compare] in the
        sense that if [compare m m' <= 0] then [compare_gen m m' <= 0]. *)

  val compare : t -> t -> int
    (** Comparison function ordering the moves (the only guarantee is
        that the order is total, besides that it is completely
        arbitrary). *)
  val to_string : t list -> string
    (** Returns a string representation of the list of moves. *)
end

module Move : MoveT


(** {2 Coordinate systems}

    Various coordinate systems with corresponding multipliction
    tables.  *)

(** The {{:http://kociemba.org/math/cubielevel.htm}cubies}
    coordinates.  *)
module Cubie :
sig
  type corner =
    | URF                               (** Up Right Front *)
    | UFL                               (** Up Front Left *)
    | ULB                               (** Up Left Back *)
    | UBR                               (** Up Back Right *)
    | DFR                               (** Down Front Right *)
    | DLF                               (** Down Left Front *)
    | DBL                               (** Down Back Left *)
    | DRB                               (** Down Right Back *)
  (** Naming of the 8 corner cubies.
      Here is a "graphical" representation of the naming scheme:
      {v
            ULB ---- UBR
             /|       /|
            / |      / |
           / DBL    /  |
         ULF ---- URF DRB
          |  /     |  /
          | /      | /
          |/       |/
         DLR ---- DFR			v}
      *)

  (** The 12 edge cubies. *)
  type edge =
    | UR                                (** Up Right *)
    | UF                                (** Up Front *)
    | UL                                (** Up Left *)
    | UB                                (** Up Back *)
    | DR                                (** Down Right *)
    | DF                                (** Down Front *)
    | DL                                (** Down Left *)
    | DB                                (** Down Back *)
    | FR                                (** Front Right *)
    | FL                                (** Front Left *)
    | BL                                (** Back Left *)
    | BR                                (** Back Right *)

  type t
    (** A scrambling of the Rubik cube (i.e. a permutation of the home
        state, i.e. an element of the Rubik group).  It is immutable. *)

  val make : corner: (corner * int) list -> edge: (edge * bool) list -> t
    (** Construct a permutation of the Rubik cube.

        The [corner] argument is the list of corners and their
        orientation (0, 1, or 2) by which URF, UFL, ULB, UBR, DFR,
        DLF, DBL, DRB are replaced (in that order).  For example, for
        the [F] move, the URF cubie is replaced by the ULF and rotated
        120 degrees clockwise, so corner is [[(ULF,1); (DLF,2);
        (ULB,0); (UBR,0); (URF,2); (DFR,1); (DBL,0); (DRB,0)]].

        The [edge] argument is the same as the [corner] but for edge
        cubies (where the orientation is [true] or [false] depending
        of whether the edge is flipped or not).  The order in which
        the replacements are given is UR, UF, UL, UB, DR, DF, DL, DB,
        FR, FL, BL, BR.

        @raise Invalid_argument if the data does not describe a valid
        permutation of the cube.  *)

  val corner : t -> corner -> corner * int
    (** [corner cube c] returns the corner and its (change of)
        orientation ([0], [1], [2] CW rotations of 120°) by which [c]
        is replaced in the [cube]. *)
  val edge : t -> edge -> edge * int
    (** [edge cube e] returns the edge and its flip state ([0]: not
        flipped, ot [1]: flipped) by which [e] is replaced in the [cube]. *)

  val move : Move.t -> t
    (** [move g] the generator [g] expressed at the cubie level.
        Thoughout this module it is considered that the action of a
        move [m] on a cube [c] is given by the {i right}
        multiplication [c] * [m] (more precisely [mul c (move m)]). *)

  val id : t
    (** The home state of the Rubik cube (i.e. the identity of the
        Rubik group).  *)

  val mul : t -> t -> t
    (** [mul g1 g2] is the group multiplication of [g1] and [g2].  It
        corresponds to permute the home cube with [g1] and then apply
        to the result the permutation represented by [g2]. *)

  val inv : t -> t
    (** [inv g] inverse permutation. *)

  val is_identity : t -> bool
    (** The permutation is the Rubik cube home state i.e., the
        identity (neutral element of the group). *)
end


(** Coordinates are "tags" labelling the right cosets Hg of a (not
    necessarily normal) subgroup H.  A fast multiplication table is
    build to handle the multiplication by a generator to the right. *)
module type Coordinate =
sig
  type t
    (** Compact form of the coordinate for fast computations. *)

  module Move : MoveT
    (** The allowed move for these coordinates. *)

  val of_cube : Cubie.t -> t
    (** [of_cube cube] returns the coordinate of the [cube].  Note
        that this function is (usually) not one-to-one, i.e. these
        coordinates only give a partial characterization of a cube.  *)

  val initialize_mul : ?dir:string -> unit -> (t -> Move.t -> t)
    (** [initialize_mul()] return a [mul] function such that [mul c m]
        applies the move [m] to the coordinate [c] i.e. right multiply
        the element [c] of the group by [m] ([c] represents a coset so
        any element of the group with coordinate [c] will give the
        same coordinates for [c * m]).  If this command is executes
        several times, it does not recompute the table.

        @param dir if the directory exists, look if it contains an
        appropriately named file and if so, assumes it contains a
        table computed by a previous run.  If it does not exist,
        create it and save the computed table.  *)

  val initialize_pruning : ?dir:string  -> unit -> (t -> int)
    (** [initialize_pruning mul] return a [prun] function such that
        [prun c] returns a lower bound for the number of moves to
        bring the cube [c] back to the goal state.

        @param dir if the directory exists, look if it contains an
        appropriately named file and if so, assumes it contains a
        table computed by a previous run.  If it does not exist,
        create it and save the computed table.   *)

  val compare : t -> t -> int
    (** Comparison function on permutations. *)
end


(** Orientation of the corner cubies (requires initialisation). *)
module CornerO : Coordinate with module Move = Move

(** Edge orientation (requires initialisation). *)
module EdgeO : Coordinate with module Move = Move

(** Corner permutation (requires initialisation). *)
module CornerP : Coordinate with module Move = Move

(** Edge permutation (requires initialisation).  It is not recommended
    to initialze it unless you have several Gb of memory. *)
module EdgeP : Coordinate with module Move = Move

(** {{:http://kociemba.org/math/twophase.htm#udslicedef}UDSlice}
    coordinates (requires initialisation).  Position of the 4 edge
    cubies (in the 12 possible positions) without taking their order
    into account. *)
module UDSlice : Coordinate with module Move = Move


(** {{:http://kociemba.org/math/twophase.htm}Phase 1} coordinates
    (requires initialisation). *)
module Phase1 :
sig
  include Coordinate with module Move = Move

  val max_moves : int
    (** Number maximum of moves to go from any position to G1 (i.e. so
        that {!Rubik.Phase1.in_G1} returns [true]).  It is known to
        be 12. *)

  val in_G1 : t -> bool
    (** Tells whether the coordinates define a cube in the subgroup G1
        generated by U, D, R2, L2, F2, B2. *)

  val in_goal : t -> bool
    (** A synonym for [in_G1]. *)
end


(** Restricted set of moves for the Phase 2 of the argorithm. *)
module Move2 :
sig
  include MoveT

  val move : t -> Cubie.t
    (** [move m] converts [m] move to the cubie level. *)
end


(** {{:http://kociemba.org/math/twophase.htm#phase2edge}Phase 2}
    coordinates (requires initialisation).  *)
module Phase2 :
sig
  include Coordinate with module Move = Move2
    (** The [move] type is local to this module (only the moves U, D,
        R2, L2, F2, B2 are permitted). *)

  val max_moves : int
    (** Number maximum of moves to go from any position in G1 to the
        identity (i.e. so that {!Rubik.Phase2.is_identity} returns
        [true]).  It is known to be 18. *)

  val is_identity : t -> bool
    (** Tells whether the coordinates define a "home" cube i.e. the
        identity element of the group. *)

  val in_goal : t -> bool
    (** A synonym for [is_identity]. *)
end
