open Rubik
open Graphics

(** Initialize the rubik state and creating a representation of it!*)

module Color :
(** Random funcion about the color face *)
sig
  type t = Red | Green | Yellow | White | Orange | Blue
      (** Color of faces *)

  val rgb_components : Graphics.color -> int * int * int
    (** [rgb_components c] returns the rgb components of [c],
        a [Graphics.color] *)

  val name : int * int * int -> t
    (** [name rgb] returns form the rgb components [rgb],
        the name of the color ([Red | Green | Yellow | White | Orange | Blue]*)

  val to_char : t -> char
    (** [to_char] returns the first letter of the name of the color
        (i.e. for [Blue] the function returns [B]).  *)
end

module Face :
  (** Abstact a face of a rubik.
      Each facelets of a face is numbered in this way :
      -------------
      | 0 | 1 | 2 |
      -------------
      | 3 | 4 | 5 |
      -------------
      | 6 | 7 | 8 |
      -------------
  *)
sig
  type t
    (** Each face is define by nine colors  *)

  val coord : int -> int * int
    (** [coord id] gives the coordinate and the absissa
        of the position [id] for working with the matrix face.
        [(0,Ø)] is the left bottom square*)

  val id : int * int -> int
    (** [id (x,y)] is the number of the facelets
        from an absissa [x] and a coordinate [y] *)

  val color_of : generator -> Color.t
    (** [color_of face] returns the color of the face [face] *)

  val color_fid : generator * int -> Color.t
    (** [color_fid (face,id)] return the color of the square number [id]
        form the face[face] *)

  val to_string : generator -> string
    (** [to_string face] gives a string representing the color 
        of each square of [face].*)
end

module Pick :
(** Functions to pick the color on a snapshot. The picking area are squares of
    14px with 30 px between them. The left bottom corner of the left bottom
    square is on (43,23)  *)
sig
  val abs : int -> int
    (** [abs x] is the absissa of the left bottom face square where [x] 
        is equal to the absissa of [Face.coord fid] *)

  val ord : int -> int
    (** [ord y] is the ordinate of the left bottom face square where [y]
        is equal to the ordinate of [Face.coord fid] *)

  val pick_point : color array array -> int -> int -> (int * int * int) list
    (** [pick_point snapshot x0 y0] return the rgb components list of points
        belonging to a 14px square. [x0] is the absissa of the left bottom
        corner of the square and [y0] the ordinate. [snapshot] is a matrix
        representing the image.*)

  val average : (int * int * int) list -> int * int * int
    (** [average list_color] returns the average of the list *)

  val take_face : generator -> int -> unit
    (** [take_face face orient] take the color of the face [face]
        with the orientation [orient] and save the data! *)
end

val find_orientation : Color.t array -> Color.t array -> int
  (** [find_orientation tf np] returns the orientation of a corner or an edge
      [tf] in the place [np] which is also a corner or an edge.
      It returns [3] if it's impossible to place the corner*)

val find : Color.t array -> (Color.t array * 'a) list -> 'a * int

val  order : Color.t array list -> (Color.t array * 'a) list -> ('a * int) list

val corner_list_replacement : unit -> (Cubie.corner * int) list
  (** [corner_list_replacement _] gives the list of the corners
      and their orientation who replace in the real cubie
      this corners : URF, UFL, ULB, UBR, DFR, DLF, DBL, DRB
      This list is used for Rubik.Cubie.make which initialize
      the cubie for a solving search*)

val edge_list_replacement : unit -> (Cubie.edge * int) list
  (** [edge_list_replacement ()] gives  the list of the edges
      and their orientation who replace in the real cubie
      this edges : UR, UF, UL, UB, DR, DF, DL, DB, FR, FL, BL, BR
      This list is used for Rubik.Cubie.make which initialize
      the cubie for a solving search*)

val create_rubik : ((Rubik.generator -> int -> unit) -> unit)  ->
  (unit -> unit) ->
  Cubie.t * Display_base.colors
  (** [create_rubik face_iter inverse_face_iter] is used for the
      initialization of the rubik colors.
      [face_iter] is a function, which given a picking color function,
      moves the rubik and execute the function giving the specification of
      the face and the rotation.
      It returns the cubie and also the color in this order :
      Up, Left, Front, Right, Back, Down.
      [inverse_face_iter] is a function, which used after [face_iter]
      put the cube in its initial position. This function is used if the
      user makes a mistake during the description of the cube.*)
