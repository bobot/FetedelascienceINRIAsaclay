open Graphics
open Printf
open Ppm
open Rubik
module D = Display_base

exception Bad_Encoding

(** Initialize the rubik state taking snapshot of the real rubik!*)

module Color =
struct
  type t = Red | Green | Yellow | White | Orange | Blue

  let all = [Red ; Green ; Yellow ; White ; Orange ; Blue]

  let to_rgb = function
    | Red -> D.red
    | Green -> D.green
    | Yellow -> D.yellow
    | White -> D.white
    | Orange -> D.orange
    | Blue -> D.blue

  let rgb_components (c:Graphics.color) =
    (c lsr 16) land 0xFF, (c lsr 8) land 0xFF, c land 0xFF

  let min a b = if (a:int) > b then b else a

  let min3 a b c = min a (min b c)

  let max a b = if (a:int) > b then a else b

  let max3 a b c = max a (max b c)

  (* according to wikipedia [http://en.wikipedia.org/wiki/HSL_color_space]
     we can find the hue from the rbg in this way *)
  let hue r g b =
    if r = g && g = b then 0
    else if r >= g && r >= b (* red is the maximun *)
    then int_of_float (360. +. 60. *. float (g - b) /. float (r - (min g b)))
      mod 360
    else if g >= r && g >= b (* green is the maximum *)
    then int_of_float(120. +. 60. *. float (b - r) /. float(g - (min b r)))
      (* blue is the maximum *)
    else int_of_float(240. +. 60. *. float (r - g) /. float (b - (min r g)))

  let lightness r g b =
    int_of_float (float ((max3 r g b + min3 r g b) / 2) /. 2.55)

  let saturation r g b =
    let diff = float(max3 r g b - min3 r g b) *. 0.5 in
    let l = float(lightness r g b) /. 100. in
    truncate(diff /. (if l <= 1.27 then l else 1. -. l))

  let name rgb =
    let (r,g,b) = rgb in
    if lightness r g b > 85 && saturation r g b < 15 then White
    else
      let h = hue r g b in
      if h <= 20 || h > 300 then Red
      else if h > 20 && h <= 45 then Orange
      else if h > 45 && h <= 75 then Yellow
      else if h > 75 && h < 180 then Green
      else Blue

  let to_char = function
    | Red -> 'R'
    | Green -> 'G'
    | Yellow -> 'Y'
    | White -> 'W'
    | Orange -> 'O'
    | Blue -> 'B'

end

module Face =
struct
  type t = Color.t array array

  let coord id = id mod 3, 2 - id / 3

  let id (x,y) = x + y * 3

  let rotation (x,y) = (2-y, x)

  let rotate (x,y) orient = match (orient mod 4) with
    | 0 -> (x,y)
    | 1 -> rotation (x,y)
    | 2 -> rotation ( rotation (x,y))
    | _ -> rotation ( rotation ( rotation (x,y)))

  (* Facelet color arrays for each face *)
  let u = Array.make_matrix 3 3 Color.Red
  let r = Array.make_matrix 3 3 Color.Red
  let f = Array.make_matrix 3 3 Color.Red
  let l = Array.make_matrix 3 3 Color.Red
  let d = Array.make_matrix 3 3 Color.Red
  let b = Array.make_matrix 3 3 Color.Red

  (* Return the face 3x3 color matrix *)
  let get = function
    | U -> u
    | R -> r
    | F -> f
    | L -> l
    | D -> d
    | B -> b

  let color_of face = (get face).(1).(1) (* center facelet *)

  (* returns the color of the element [id] of the face [face] *)
  let color_fid (face,id) =
    let (x,y) = coord id in
    (get face).(x).(y)

  let name face = match face with
    | U -> "Up"
    | R -> "Right"
    | F -> "Front"
    | L -> "Left"
    | D -> "Down"
    | B -> "Back"

  let to_string face =
    let row r =
      "\n-------------\n|"
      ^ Array.fold_right (fun f s -> sprintf "%s %c |" s (Color.to_char f)) r ""
    in
    (Array.fold_right (fun r s -> s ^ row r) (get face) "")
    ^ "\n-------------\n"
end

module Pick =
struct
  let abs x = 43 + 30 * x

  let ord y = 23 + 30 * y

  let pick_point snapshot x0 y0 =
    let rec pick_y y ret =
      let rec pick_x x retour = match x with
        |3 -> retour
        |_ ->
           pick_x (x+1)
             (Color.rgb_components
                (snapshot.(Array.length snapshot -y0- y*7).(x0 + x*7))
              :: retour)
      in
      match y with
      |3 -> ret
      |_ -> pick_y (y+1) ((pick_x 0 []) :: ret) in
    List.concat (pick_y 0 [])


  let ( +! ) (x1,y1,z1) (x2,y2,z2) = (x1 + x2, y1 + y2, z1 + z2)

  let ( /! ) (x,y,z) a = (x/a ,y/a, z/a)

  (* compute the average vector of a vector list *)
  let average list_color =
    let rec itern lc sum number_el = match lc with
      |[] -> sum /! number_el
      |el :: li -> itern li (sum +! el) (number_el+1)
    in itern list_color (0,0,0) 0

  (* take face with taking the snapshot manually *)
  let tf snapshot_file face orient =
    printf "press a key for picking color on the face %s\n%!"
      (Face.to_string face);
    ignore(wait_next_event [Key_pressed]);
    let img = Ppm.as_matrix_exn snapshot_file in
    let fill_matrix_square x y =
      let (i,j) = Face.rotate (x,y) orient in
      (Face.get face).(x).(y)
      <- Color.name (average (pick_point img (abs i) (ord j)));
    in
    Array.iter (fun x ->
                  Array.iter (fun y -> fill_matrix_square x y) [|0;1;2|]
               ) [|0;1;2|]
      (* used for the graphical selection of the color *)


  (** Tells whether [(x,y)] is in the rectangle whose bottom left
      corner is [(x0,y0)] and width (resp. height) is [w] (resp. [h]). *)
  let in_rectangle x y (x0,y0, w,h) =
    x0 <= x && x <= x0 + w && y0 <= y && y <= y0 + h

  (** Draw the rectangle whose bottom left corner is [(x0,y0)] and
      width (resp. height) is [w] (resp. [h]) with inner [color]. *)
  let draw_rectangle ?(thick=false) (x0,y0, w,h) color =
    set_line_width (if thick then 5 else 1);
    set_color color;
    fill_rect x0 y0 w h;
    set_color black;
    draw_rect x0 y0 w h

  let erase_rectangle ?(thick=false) (x0,y0, w,h) =
    set_line_width (if thick then 5 else 1);
    set_color background;
    fill_rect x0 y0 w h

  let facelets_coord =
    List.fold_left begin fun c i ->
      List.fold_left (fun c j -> (i,j) :: c) c [0;1;2]
    end [] [0;1;2]

  let all_colors =
    fst(List.fold_left (fun (l, i) c -> (i,c) :: l, i+1) ([],0) Color.all)

  let man_take_face face orient =
    let side = 50 in
    let x0 = side
    and y0 = side + 20 in
    (* State *)
    let current_color = ref Color.Red in
    let current_color_rect = ref (-1,-1,0,0) in
    let not_quit = ref true in

    let oriented_mface = Array.make_matrix 3 3 Color.Red in
    let mface = Face.get face in
    for x = 0 to 2 do
      for y = 0 to 2 do
        let (i,j) = Face.rotate (x,y) orient in
        oriented_mface.(x).(y) <- mface.(i).(j)
      done;
    done;

    clear_graph();
    set_window_title "Rubik: enter cube colors";
    let make_rect i j = (x0 + i * side, y0 + j * side, side, side)
    and set_facelet_color i j r () =
      draw_rectangle r (Color.to_rgb !current_color);
      oriented_mface.(i).(j) <- !current_color  in
    let quit_text = "Face suivante" in
    let (w,h) = text_size quit_text in
    let quit_rect = (x0, y0 - 30 - h, w + 10, h + 10) in
    let draw_quit () =
      draw_rectangle quit_rect 0xcfdedd;
      moveto (x0 + 5) (y0 - 25 - h);
      draw_string quit_text in
    let color_rect i =
      let d = 3 * (side / 2) in
      (x0 + 4 * side + (i / 3) * d, y0 + (i mod 3) * d, side, side)
    and change_color rect c () =
      erase_rectangle !current_color_rect ~thick:true;
      draw_rectangle !current_color_rect (Color.to_rgb !current_color);
      current_color := c;
      current_color_rect := rect;
      draw_rectangle rect (Color.to_rgb c) ~thick:true in
    (* [(rectangle, associated action)] *)
    let buttons init =
      (quit_rect, (fun () -> not_quit := false))
      :: List.map (fun (i,c) ->
                     let r = color_rect i in (r, change_color r c)
                  ) all_colors
      @ List.map (fun (i,j) ->
                    let r = make_rect i j in (r,
                           ( if init then
                               (fun () -> draw_rectangle r
                                  (Color.to_rgb oriented_mface.(i).(j)))
                             else (set_facelet_color i j r)))
                 ) facelets_coord in
    (* Draw all buttons.  FIXME: on windows the initial state does not
       always shows up properly.  Is it an interaction with the Unix
       module connecting on COMxx ??? *)
    auto_synchronize false;
    (* draw the face for the first time *)
    List.iter (fun (r,f) -> f()) (buttons true);
    draw_quit();
    auto_synchronize true;
    (* Start graphical interaction *)
    not_quit := true;
    while !not_quit do
      let st = wait_next_event [Button_down; Key_pressed] in
      if st.button then
        List.iter (fun (r,f) ->
                     if in_rectangle st.mouse_x st.mouse_y r then f()
                  ) (buttons false)
    done;
    for x = 0 to 2 do
      for y = 0 to 2 do
        let (i,j) = Face.rotate (x,y) orient in
        mface.(i).(j) <- oriented_mface.(x).(y)
      done;
    done;
    printf "%s (orient: %i)%s\n%!" (Face.name face) orient (Face.to_string face)
   (*  close_graph () *)
  ;;

  let take_face face orient =
    let webcam = Snapshot.start () in
    let snapshot = Snapshot.take webcam in
    Snapshot.stop webcam;
    let fill_matrix_square x y =
      let (i,j) = Face.rotate (x,y) orient in
      (Face.get face).(x).(y)
      <- Color.name (average (pick_point snapshot (abs i) (ord j)));
    in
    Array.iter (fun x ->
                  Array.iter (fun y -> fill_matrix_square x y) [|0;1;2|]
               ) [|0;1;2|]
end

(* list of all the corner of the cube *)
let corner_list = [ Cubie.URF; Cubie.UFL; Cubie.ULB; Cubie.UBR;
                    Cubie.DFR; Cubie.DLF; Cubie.DBL; Cubie.DRB ]

(* list of all the edge of the cube *)
let edge_list = [ Cubie.UR; Cubie.UF; Cubie.UL; Cubie.UB;
                  Cubie.DR; Cubie.DF; Cubie.DL; Cubie.DB;
                  Cubie.FR; Cubie.FL; Cubie.BL; Cubie.BR]

(* definition of the face designing the corner in the clockwise sens *)
let corner_set = function
  | Cubie.URF -> [| U; R; F |]
  | Cubie.UFL -> [| U; F; L |]
  | Cubie.ULB -> [| U; L; B |]
  | Cubie.UBR -> [| U; B; R |]
  | Cubie.DFR -> [| D; F; R |]
  | Cubie.DLF -> [| D; L; F |]
  | Cubie.DBL -> [| D; B; L |]
  | Cubie.DRB -> [| D; R; B |]

(* definition of the face designing the edge in the clockwise sens *)
let edge_set = function
  | Cubie.UR -> [| U; R |]
  | Cubie.UF -> [| U; F |]
  | Cubie.UL -> [| U; L |]
  | Cubie.UB -> [| U; B |]
  | Cubie.DR -> [| D; R |]
  | Cubie.DF -> [| D; F |]
  | Cubie.DL -> [| D; L |]
  | Cubie.DB -> [| D; B |]
  | Cubie.FR -> [| F; R |]
  | Cubie.FL -> [| F; L |]
  | Cubie.BL -> [| B; L |]
  | Cubie.BR -> [| B; R |]


(* return for each corner his design in term of face - id *)
let corner_def = function
  | Cubie.URF -> [| (U,8); (R,0); (F,2) |]
  | Cubie.UFL -> [| (U,6); (F,0); (L,2) |]
  | Cubie.ULB -> [| (U,0); (L,0); (B,2) |]
  | Cubie.UBR -> [| (U,2); (B,0); (R,2) |]
  | Cubie.DFR -> [| (D,2); (F,8); (R,6) |]
  | Cubie.DLF -> [| (D,0); (L,8); (F,6) |]
  | Cubie.DBL -> [| (D,6); (B,8); (L,6) |]
  | Cubie.DRB -> [| (D,8); (R,8); (B,6) |]

(* return for each corner his design in term of face - id *)
let edge_def = function
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

(* returns true if the corner or edge with the orientation [orient]
   fit in the place (which is given by a corner or a an edge*)
let harmony tf np orient =
  let lgth = Array.length np in
  let rec iter ret i =
    if i = lgth then ret
    else iter (ret && (np.(i) = tf.((i+orient) mod lgth))) (i+1)
  in iter true 0

let find_orientation tf np =
  let lgth = Array.length tf in
  let rec iter it =
    if it = lgth then 3
    else
      if harmony tf np it then it
      else iter (it +1)
  in iter 0

let find tf new_position =
  (* finds the corner or the edge in the list [new_position] where fit the
     element [tf] *)
  let rec iter np_l = match np_l with
    |[] -> raise Bad_Encoding
    |(el_color, el_return) :: li ->
       let orient = find_orientation tf el_color in
       if orient = 0 || orient = 1 || orient = 2 then el_return, orient
       else iter li
  in iter new_position

(* Find the new position and orientation of each elements of the list
   [to_find] in the list [new_position]*)
let order to_find new_position =
  let rec iter ret tf = match tf with
    |[] -> List.rev ret
    |el :: li -> iter ((find el new_position) :: ret) li
  in iter [] to_find

let corner_list_replacement _ =
  let corner_new_position = List.map (fun corner ->
                                Array.map (fun face ->
                                             Face.color_of face)
                                  (corner_set corner), corner) corner_list in
  let corner_to_find = List.map (fun corner ->
                                 Array.map (fun face_id ->
                                              Face.color_fid face_id)
                                   (corner_def corner)) corner_list in
  order corner_to_find corner_new_position

let edge_list_replacement _ =
  let edge_new_position = List.map (fun edge ->
                                Array.map (fun face ->
                                             Face.color_of face)
                                  (edge_set edge), edge) edge_list in
  let edge_to_find = List.map (fun edge ->
                                 Array.map (fun face_id ->
                                              Face.color_fid face_id)
                                   (edge_def edge)) edge_list in
  order edge_to_find edge_new_position

let try_create_rubik face_iter =
  face_iter (Pick.man_take_face);
  let corner_list_ordered = corner_list_replacement () in
  let edge_list_ordered = edge_list_replacement () in
  let elo = List.map (fun (a,i) -> (a, i = 1)) edge_list_ordered in
  let cubie = Cubie.make corner_list_ordered elo in
  (cubie,
   { D.color_F = Color.to_rgb(Face.color_of F);
     color_B = Color.to_rgb(Face.color_of B);
     color_L = Color.to_rgb(Face.color_of L);
     color_R = Color.to_rgb(Face.color_of R);
     color_U = Color.to_rgb(Face.color_of U);
     color_D = Color.to_rgb(Face.color_of D);
     color_lines = black }
  )

let rec create_rubik face_iter return_face_init =
  try
    try_create_rubik face_iter
  with Invalid_argument _ | Bad_Encoding ->
        clear_graph();
        let error_text = "Erreur d'encodage, veuillez corriger" in
        draw_string error_text;
        ignore(wait_next_event [Key_pressed]);
        return_face_init ();
        create_rubik face_iter return_face_init

(*
let () =
  (*Pick.man_take_face U 0 *)
  printf "t\n%!";
  printf "-%s\n%! " (Color.to_string (Face.color_fid (U,8)));
  let corner_list_ordered = corner_list_replacement () in
  let edge_list_ordered = edge_list_replacement () in
  let elo = List.map (fun (a,i) -> (a, (i = 1))) edge_list_ordered in
  let cubie = Cubie.make corner_list_ordered elo in
  let x0 = 10
  and y0 = 10
  and len_sq = 30 in
  let colors = (Color.to_rgb (Face.color_of U),
                Color.to_rgb (Face.color_of L),
                Color.to_rgb (Face.color_of F),
                Color.to_rgb (Face.color_of R),
                Color.to_rgb (Face.color_of B),
                Color.to_rgb (Face.color_of D)) in
  open_graph ("");
  printf "%s\n" (Face.to_string U);
  printf "%s\n" (Face.to_string L);
  printf "%s\n" (Face.to_string F);
  printf "%s\n" (Face.to_string R);
  printf "%s\n" (Face.to_string B);
  printf "%s\n" (Face.to_string D);
  (* ce pl *)
  let o =  find_orientation
    (Array.map (fun face_id -> Face.color_fid face_id)
       (corner_def Cubie.UBR))
    (Array.map (fun face -> Face.color_of face)
       (corner_set Cubie.UFL)) in
  printf "or - %i\n%!" o;
  Display.cube x0 y0 colors len_sq cubie;
  ignore(wait_next_event [Button_down])*)
