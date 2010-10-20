(* File: display.ml

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

open Graphics
open Rubik

type color6 = color * color * color * color * color * color

let pi = 4. *. atan(1.)

let radians_of_degrees angle = float_of_int(angle) *. pi /. 180.

let round x = truncate(x +. 0.5) (* rounds a float *)

type vector = int * int (* Represents a vector of a 2D plane *)
let ( +! ) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
let ( *! ) a (x,y) = (a * x, a * y)


(* Characteristics of one of the 6 faces of the cube.  The bottom left
   corner *)
type face = {
  xy : vector; (* The coordinates of the bottom left corner of the face *)
  v1 : vector; (* The bottom edge vector, its length = facelet "width" *)
  v2 : vector; (* The left edge vector, its length = facelet "height" *)
}

let corner_list =
  [ Cubie.URF; Cubie.UFL; Cubie.ULB; Cubie.UBR;
    Cubie.DFR; Cubie.DLF; Cubie.DBL; Cubie.DRB]
let edge_list =
  [ Cubie.UR; Cubie.UF; Cubie.UL; Cubie.UB;
    Cubie.DR; Cubie.DF; Cubie.DL; Cubie.DB;
    Cubie.FR; Cubie.FL; Cubie.BL; Cubie.BR ]

(* Associate to a corner cubie the 3 faces and number of facelets that
   are visible.  The facelets for a given face are numbered in the
   following way:
   -------------
   | 0 | 1 | 2 |
   -------------
   | 3 | 4 | 5 |
   -------------
   | 6 | 7 | 8 |
   -------------  *)
let faces_of_corner = function
  | Cubie.URF -> [| (U,8); (R,0); (F,2) |]
  | Cubie.UFL -> [| (U,6); (F,0); (L,2) |]
  | Cubie.ULB -> [| (U,0); (L,0); (B,2) |]
  | Cubie.UBR -> [| (U,2); (B,0); (R,2) |]
  | Cubie.DFR -> [| (D,2); (F,8); (R,6) |]
  | Cubie.DLF -> [| (D,0); (L,8); (F,6) |]
  | Cubie.DBL -> [| (D,6); (B,8); (L,6) |]
  | Cubie.DRB -> [| (D,8); (R,8); (B,6) |]

let faces_of_edges = function
  | Cubie.UR -> [| (U,5); (R,1) |]
  | Cubie.UF -> [| (U,7); (F,1) |]
  | Cubie.UL -> [| (U,3); (L,1) |]
  | Cubie.UB -> [| (U,1); (B,1) |]
  | Cubie.DR -> [| (D,5); (R,7) |]
  | Cubie.DF -> [| (D,1); (F,7) |]
  | Cubie.DL -> [| (D,3); (L,7) |]
  | Cubie.DB -> [| (D,7); (B,7) |]
  | Cubie.FR -> [| (F,5); (R,3) |]
  | Cubie.FL -> [| (F,3); (L,5) |]
  | Cubie.BL -> [| (B,5); (L,3) |]
  | Cubie.BR -> [| (B,3); (R,5) |]

(* substraction x - y modulo 3 *)
let sub3 x y =
  assert(0 <= x && x <= 2 && 0 <= y && y <= 2);
  if y <= x then x - y else 3 + x - y

(* substraction modulo 2 *)
let sub2 x y =
  assert(0 <= x && x <= 1 && 0 <= y && y <= 1);
  if y <= x then x - y else 2 + x - y


let cube ?(pen=black) ?(angle=45) x0 y0 (c_U, c_L, c_F, c_R, c_B, c_D) len_sq =
  let angle = radians_of_degrees angle in
  (* Projection on the image of the basis of IR^3 *)
  let e1 = (len_sq, 0)
  and e2 = (round(float len_sq *. cos angle),
            round(float len_sq *. sin angle))
  and e3 = (0, len_sq) in
  let len_face = 3 * len_sq in
  (* The 6 faces of the cube: Front, Back, Left, Right, Up, Down. *)
  let face_R = { xy = (x0 + 2 * len_face, y0 + len_face); v1 = e2; v2 = e3 } in
  let face_F = { xy = (x0 + len_face, y0 + len_face);     v1 = e1; v2 = e3 }
  and face_B = { xy = face_R.xy +! 3 *! e2;               v1 = e1; v2 = e3 }
  and face_L = { xy = (x0, y0 + len_face);                v1 = e1; v2 = e3 }
  and face_U = { xy = (x0 + len_face, y0 + 2 * len_face); v1 = e1; v2 = e2 }
  and face_D = { xy = (x0 + len_face, y0);                v1 = e1; v2 = e3 } in
  let characteristics face = match  face with
      (*Giving the feature of the face [face]*)
    | U -> face_U
    | L -> face_L
    | F -> face_F
    | R -> face_R
    | B -> face_B
    | D -> face_D
  in
  let color_of face = match  face with
      (*Giving the color of the face [face]*)
    | U -> c_U
    | L -> c_L
    | F -> c_F
    | R -> c_R
    | B -> c_B
    | D -> c_D
  in
  let draw_facelet (face, n) color =
    (* Draw the facelet numbered [n] (see [faces_of_corner] for the
       numbering scheme) of the face [face] in the color [color] *)
    let face = characteristics face in
    let xy0 = face.xy +! (n mod 3) *! face.v1 +! (2 - n / 3) *! face.v2 in
    let facelet = [| xy0;  xy0 +! face.v1; xy0 +! face.v1 +! face.v2;
                     xy0 +! face.v2 |] in
    set_color color;
    fill_poly facelet;
    set_color pen;
    draw_poly facelet
  in
  let draw_corner corner (replacement, orientation) =
    (* Draws the corner [structure] at the position [position] with
       the orientation [orientation] *)
    let r = faces_of_corner replacement in
    Array.iteri (fun i facelet ->
                   let color = color_of (fst r.(sub3 i orientation)) in
                   draw_facelet facelet color
                ) (faces_of_corner corner)
  in
  let draw_edge corner (replacement, flipped) =
    (* Draws the edge [structure] at the position [position] who is
       [flipped] or not! *)
    let r = faces_of_edges replacement in
    Array.iteri (fun i facelet ->
                   let color = color_of (fst r.(sub2 i flipped)) in
                   draw_facelet facelet color
                ) (faces_of_edges corner)
  in
  fun cube ->
    (* Draw the center cubies *)
    draw_facelet (U,4) c_U;
    draw_facelet (R,4) c_R;
    draw_facelet (L,4) c_L;
    draw_facelet (F,4) c_F;
    draw_facelet (B,4) c_B;
    draw_facelet (D,4) c_D;
    (* Draw the other cubies *)
    List.iter (fun c -> draw_corner c (Cubie.corner cube c)) corner_list;
    List.iter (fun e -> draw_edge e (Cubie.edge cube e)) edge_list

(* test -- temporary *)
(*open Printf
let () =
  let x0 = 10
  and y0 = 10
  and len_sq = 30 in
  let img_width = x0 + 24 * len_sq in
  let img_high = y0 + 9 * len_sq in
  open_graph (sprintf " %ix%i" img_width img_high);
  set_color (rgb 219 219 219);
  fill_rect 0 0 img_width img_high;

  let colors = (blue, magenta, yellow, red, white, green) in
  cube x0 y0 colors len_sq Cubie.id;
  cube (x0 + 12 * len_sq) y0 colors len_sq (Cubie.move (Move.make (B, 1)));
  ignore(wait_next_event [Button_down])
*)
