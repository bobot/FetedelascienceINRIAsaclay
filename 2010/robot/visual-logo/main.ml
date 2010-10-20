(* Visual programs' syntax. *)

type direction = [
  | `Up
  | `Left
  | `Right
]

type prog = direction list

type interface_command = [ 
| `Up 
| `Left 
| `Right 
| `Pop 
| `Compile 
| `Execute 
| `Background 
]

(* Program constants. *)

let max_prog_len       = 10
let width	       = 1024
let height	       = 768
let image_filenames    = [ 
  "go.bmp", 50, 50; 
  "turn_left.bmp", 50, 50; 
  "turn_right.bmp", 50, 50; 
  "cancel.bmp", 200, 50; 
  "compile.bmp", 200, 50; 
  "run.bmp", 200, 50; 
  "background.bmp", 880, 80; 
]
let image_directory    = "images"
let step_button_x      = width  / 4
let step_distance_x    = width  / 10
let step_button_y      = height / 3
let command_button_x   = width / 10
let command_distance_x = width / 3
let command_button_y   = 4 * height / 5
let background_x       = width / 10
let background_y       = height / 2 
let prog_step_x	       = background_x + 60
let prog_step_y	       = background_y + 15
let prog_step_distance = width / 10

(* Visual stuff. *)

let random_color () = 
  (Random.int 255, Random.int 255, Random.int 255)

let image_width img = 
  let (w, _, _) = Sdlvideo.surface_dims img in 
  w

let image_height img = 
  let (_, h, _) = Sdlvideo.surface_dims img in 
  h

let reduce img dimx dimy = 
  let rimg = Sdlvideo.create_RGB_surface_format img [] dimx dimy in
  let sx x = x * image_width img / dimx 
  and sy y = y * image_height img / dimy in
  for i = 0 to dimx - 1 do
    for j = 0 to dimy - 1 do
      Sdlvideo.put_pixel rimg i j (Sdlvideo.get_pixel img (sx i) (sy j))
    done
  done;
  rimg

let load_bitmap (filename, dimx, dimy) = 
  try
    reduce 
      (Sdlvideo.load_BMP (Filename.concat image_directory filename)) 
      dimx dimy
  with _ -> failwith (Printf.sprintf "%s not found." filename)

let buttons = 
  [ `Up; `Left; `Right; `Pop; `Compile; `Execute; `Background ]

let screen = 
  Sdl.init [`VIDEO];
  Sdlvideo.set_video_mode width height []

let images = 
  List.combine buttons (List.map load_bitmap image_filenames)

let display_image (xpos, ypos) img = 
  let pos = Sdlvideo.rect xpos ypos (image_width img) (image_height img) in
  Sdlvideo.blit_surface ~dst_rect:pos ~src:img ~dst:screen ()
 
let hbox i xstart xstep y = 
  (xstart + i * xstep, y) 

let step_button_layout i = 
  hbox i step_button_x step_distance_x step_button_y

let command_button_layout i = 
  hbox i command_button_x command_distance_x command_button_y

let layout = function
  | `Up         -> step_button_layout 0
  | `Left       -> step_button_layout 1
  | `Right      -> step_button_layout 2
  | `Pop        -> command_button_layout 0
  | `Compile    -> command_button_layout 1
  | `Execute    -> command_button_layout 2
  | `Background -> (background_x, background_y)

let image_of what = List.assoc what images

let display what = display_image (layout what)

let display' what = display what (image_of what)

let _init = 
  let white = Sdlvideo.map_RGB screen (255, 255, 255) in
  Sdlvideo.fill_rect screen white;
  List.iter (function (kind, img) -> display kind img) images

let msg msg = 
  Printf.printf "MSG: %s\n%!" msg

let display_step i =
  let pos = hbox i prog_step_x prog_step_distance prog_step_y in
  fun (s : direction) -> 
    display_image pos 
      (image_of (s : direction :> interface_command))

let display_prog = 
  let last_prog = ref None in 
  fun (prog : prog) -> 
    let do_it () = 
      last_prog := Some prog; 
      display' `Background;
      ignore (List.fold_left (fun i s -> display_step i s; i + 1) 0 
		(List.rev prog))
    in
    match !last_prog with 
      | None -> do_it ()
      | Some prog' when prog <> prog' -> do_it ()
      | Some _ -> ()

let step_limit = 8
let push_step s prog = 
  if List.length prog < step_limit then
    s :: prog
  else 
    prog

let pop_step = function
  | _ :: s -> s
  | [] -> []

type command = 
  | PushStep of direction
  | PopStep
  | Compile
  | Execute
  | Nothing

let on_tmp_file contents f =
  let fname = Filename.temp_file "robot" "" in
  let cout = open_out fname in
  Buffer.output_buffer cout contents;
  close_out cout;
  f fname

let compile prog = 
  let prog = List.rev prog in
  let b = Buffer.create 13 in
  let add_cmd s = Buffer.add_string b (Printf.sprintf "%s ();\n" s) in
  let cmds = List.map (function 
			 | `Left  -> "tourne_gauche"
			 | `Right -> "tourne_droite"
			 | `Up    -> "avance") prog
  in
  List.iter add_cmd cmds;
  let status = 
    on_tmp_file b 
      (fun f -> Sys.command (Printf.sprintf "./compile %s" f));
  in
  if status = 0 then Some () else None

let bluetooth_addr = "00:16:53:0A:AF:E0"
let execute cprog  = 
  ignore (Sys.command (Printf.sprintf "./out.byte %s" bluetooth_addr))

let wait_for_event () = 
  let status = Sdlevent.wait_event () in
  let in_rect (posx, posy, dimx, dimy) (mx, my) = 
    posx <= mx && mx <= posx + dimx && posy <= my && my <= posy + dimy
  in
  let clicked_button m (button, image) =
    let (bx, by) = layout button in 
    in_rect 
      (bx, by, image_width image, image_height image) 
      (m.Sdlevent.mbe_x, m.Sdlevent.mbe_y)
  in
  try 
    match status with
      | Sdlevent.MOUSEBUTTONDOWN m -> 
	  begin match fst (List.find (clicked_button m) images) with 
	    | `Up         -> PushStep `Up
	    | `Left       -> PushStep `Left
	    | `Right      -> PushStep `Right
	    | `Pop        -> PopStep 
	    | `Execute    -> Execute
	    | `Compile    -> Compile
	    | `Background -> Nothing
	  end
      | Sdlevent.QUIT -> Sdl.quit (); exit 0
      | _ -> Nothing
  with Not_found -> Nothing

let rec loop ((prog : prog), cprog) = 
  Sdlvideo.flip screen;
  display_prog prog;
  match wait_for_event () with
    | PushStep s -> loop (push_step s prog, None)
    | PopStep    -> loop (pop_step prog, None)
    | Compile    -> loop (prog, compile prog)
    | Nothing    -> loop (prog, cprog)
    | Execute    -> 
	match cprog with
	  | None -> msg "Vous devez d'abord compiler."
	  | Some cprog -> execute cprog; loop ([], None)
  
let _ = 
  loop ([], None); 
  Sdl.quit ()
