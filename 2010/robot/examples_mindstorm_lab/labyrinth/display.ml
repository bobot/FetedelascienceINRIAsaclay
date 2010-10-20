(* File: display.ml

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

module type T =
sig
  include Labyrinth.T
  val success : unit -> unit
  val failure : unit -> unit
  val set_current_path : dir list -> unit
  val get_current_path : unit -> dir list
  val close_when_clicked : unit -> unit
end

open Printf
open Graphics

let (size_x, size_y) = (720,720)
let (x0,y0) = (360, 350)      (* initial position *)
let square_length = 40 (* pixels, excluding walls *)
let robot_color = rgb 65 40 189
let wall_thickness = 2 (* pixels; wall are twice as thick *)
let wall_color = black
let explored_color = rgb 166 227 147    (* green *)
let cross_road_color = red
let dismissed_color = rgb 18 152 52
let path_color = rgb 49 147 192
let laby_structure = rgb 171 183 227
let text_fonts =
  [ "-*-times new roman-medium-r-normal--100-*-*-*-p-*-iso8859-1";
    "lucidasans-bolditalic-24"]
let goal_color = yellow
let text_success = "Trouvé !"
let failure_color = rgb 253 129 129
let text_failure = "Pas de sortie !"

let dx = square_length + 2 * wall_thickness
let dy = dx

module Make(L : Labyrinth.T) : T =
struct
  include L

  let rec try_set_font = function
    | [] -> ()
    | font :: tl -> (try set_font font with _ -> try_set_font tl)

  (** @param color : force the square color (for example for the
      "success" square). *)
  let draw_square ?color ((x,y) as xy) =
    let px = x0 + x * dx + wall_thickness
    and py = y0 + y * dy + wall_thickness in
    set_color (match color with
               | Some c -> c
               | None ->
                   match status xy with
                   | `Explored | `Cross_roads -> explored_color
                   | `Non_explored -> background
                   | `Dismissed -> dismissed_color);
    fill_rect px py square_length square_length;
    if status xy = `Cross_roads then begin
      (* Add a special mark on "crossroads" *)
      set_color cross_road_color;
      moveto px py;  rlineto square_length square_length;
      moveto px (py + square_length);  rlineto square_length (- square_length)
    end

  (** Draw the robot according to its state in [L]. *)
  let draw_robot ?color () =
    draw_square ?color (robot_pos());
    let d = 3 in
    let d2 = 2 * d in
    let w = square_length / 2 in
    let poly = match robot_dir() with
      | `N -> [| (d2,d); (w, square_length - d); (square_length - d2,d) |]
      | `S -> [| (d2, square_length - d); (w,d);
                 (square_length - d2, square_length - d) |]
      | `W -> [| (square_length - d, d2); (d, w);
                 (square_length - d, square_length - d) |]
      | `E -> [| (d,d2); (square_length - d, w); (d, square_length - d) |] in
    let (x,y) = robot_pos() in
    let px = x0 + x * dx + wall_thickness
    and py = y0 + y * dy + wall_thickness in
    set_color robot_color;
    fill_poly (Array.map (fun (x,y) -> (x + px, y + py)) poly)
  ;;

  let () =
    (* Initialize the graphic window *)
    open_graph(sprintf " %ix%i" size_x size_y);
    set_window_title "Labyrinth - visualisation";
    (* Draw lines to suggest the labyrinth *)
    set_color laby_structure;
    for x = -7 to 7 do
      moveto (x0 + x * dx) (y0 - 7 * dy);  rlineto 0 (14 * dy)
    done;
    for y = -7 to 7 do
      moveto (x0 - 7 * dx) (y0 + y * dy);  rlineto (14 * dx) 0
    done;
    (* Draw initial robot *)
    set_line_width 1;
    draw_robot()

  let draw_wall (rx,ry) (d: dir) b =
    set_color (if b then wall_color else explored_color);
    let px = x0 + rx * dx and py = y0 + ry * dy in
    match d with
    | `N ->
        let x = px + wall_thickness
        and y = py + wall_thickness + square_length in
        fill_rect x y square_length (2 * wall_thickness)
    | `S ->
        let x = px + wall_thickness
        and y = py - wall_thickness in
        fill_rect x y square_length (2 * wall_thickness)
    | `W ->
        let x = px - wall_thickness
        and y = py + wall_thickness in
        fill_rect x y (2 * wall_thickness) square_length
    | `E ->
        let x = px + wall_thickness + square_length
        and y = py + wall_thickness in
        fill_rect x y (2 * wall_thickness) square_length

  (* Add a notion of "current path" that the robot is following to go
     back to a crossroad.  It is necessary to keep it in memory
     because one also updates neighboring squares. *)

  let current_path = ref []

  let set_current_path p = current_path := p

  let get_current_path () = !current_path

  let draw_path dir_path =
    let pos = robot_pos() in
    let path (l,p) d = let p' = Coord.move p d in (p' :: l, p') in
    let squares, _ = List.fold_left path ([pos], pos) dir_path in
    let mid_square = wall_thickness + square_length / 2 in
    let xy = List.map (fun (x,y) ->
                         (x0 + x * dx + mid_square, y0 + y * dy + mid_square)
                      ) squares in
    set_line_width 3;
    set_color path_color;
    draw_poly_line (Array.of_list xy);
    set_line_width 1


  (* Redefine some functions of [L] to add a graphical animation *)

  (* @override *)
  let set_wall (d: dir_rel) b =
    L.set_wall d b;
    draw_wall (robot_pos()) (abs_dir d) b;
    (* New walls may change the status of the current square *)
    draw_robot();
  ;;

  (* @override *)
  let turn d =
    L.turn d;
    draw_robot()
  ;;

  (* @override *)
  let move ?affects () =
    L.move () ~affects:begin fun dismissed boundary ->
      List.iter draw_square dismissed;
      List.iter draw_square boundary;
      (* The first move of the path has just been made, do not draw it *)
      if !current_path <> [] then draw_path (List.tl !current_path);
      let pos = L.robot_pos() in
      List.iter begin fun (d,_) ->
        if L.wall_on pos d = `False then draw_wall pos d false
      end (Coord.nbh pos);
      draw_robot();

      match affects with None -> () | Some f -> f dismissed boundary
    end
  ;;

  (* New functions
   ***********************************************************************)

  let draw_final color text =
    (* redraw_nbh(robot_pos()); *)
    (* There should be no path but, if we modified the labyrinth while
       the robot was discovering it, one would still like to see that
       path that we were trying to follow. *)
    if !current_path <> [] then draw_path (List.tl !current_path);
    draw_robot ~color ();
    try_set_font text_fonts; (* ignore nonexistent font *)
    set_color black;
    let (w,h) = text_size text in
    moveto x0 y0;  rmoveto (-w / 2) (-h /2);  draw_string text

  let success () = draw_final goal_color text_success
  let failure () = draw_final failure_color text_failure

  let close_when_clicked () =
    ignore(wait_next_event [Button_down])
end


(* Local Variables: *)
(* compile-command: "make -k display.cmo" *)
(* End: *)

