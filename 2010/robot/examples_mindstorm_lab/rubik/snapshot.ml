open Graphics
(* File: snapshot.ml

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


(* FIXME: this file needs some cleanup *)

type color = int

let imagemagick_convert = "convert"


let convert fname1 fname2 =
  let cmd = imagemagick_convert ^ " " ^ fname1 ^ " " ^ fname2 in
  Printf.eprintf "### %s\n%!" cmd;
  ignore(Sys.command cmd)

(* Helpers
 ***********************************************************************)

let copy fname1 fname2 =
  let buf = String.create 8192 in
  let fin = open_in_bin fname1 in
  let fout = open_out_bin fname2 in
  let read = ref(-1) in
  while !read <> 0 do
    read := input fin buf 0 8192;
    output fout buf 0 !read;
  done;
  close_in fin;
  close_out fout

let must_wait_longer = function
  | None -> true
  | Some st ->
      st.Unix.st_size = 0
      && (st.Unix.st_mtime -. st.Unix.st_atime <= 10.)

let rec wait_for_file fname =
  (* Wait (unfortunately busily) that w.png exists AND is > 0 bytes
     AND is a few seconds old (for automatic color adjustments). *)
  let st = try Some(Unix.stat fname) with _ -> None in
  if must_wait_longer st then begin
    prerr_endline "wait for webcam";
    Unix.sleep 1;
    wait_for_file fname
  end
;;

let exchange_rb img =
  let ret = Array.make_matrix (Array.length img) (Array.length img.(0)) red in
  let rgb_components c =
    (c lsr 16) land 0xFF,
    (c lsr 8) land 0xFF,
    c land 0xFF in
  for i = 0 to (Array.length img) - 1
  do
    for j = 0 to (Array.length img.(0)) -1
    do
      let (r,g,b) = rgb_components img.(i).(j) in
      ret.(i).(j) <- Graphics.rgb b g r
    done;
  done;
  ret


(************************************************************************)

IFDEF WIN32 THEN
(* This approach saves snapshots in a file that we can read in
   (execute "vlc -p image" for more info).  The "-I rc" starts the
   remote-control interface, so no display is shown and we have an
   easy way to quit.  *)
let vlc_remote = "c:/\"Program files\"/VideoLAN/VLC/vlc -I rc \
        dshow:// :dshow-adev=\"none\" --vout=image --image-out-replace \
        --image-out-format=png --image-out-prefix=rubik"
let vlc = "c:/Program files/VideoLAN/VLC/vlc"
let vlc_args fname =
  [| "--intf=rc"; "dshow://"; "--vout=image"; "--dshow-adev=\"none\"";
     "--image-out-replace"; "--image-out-format=png";
     "--image-out-prefix=" ^ fname |]
ELSE
IFDEF MACOS THEN

(* MacOS
   ----------------------------------------------------------------------

   vlc does not connect to a webcam *)
let vlc = "/usr/bin/vlc"
let vlc_args fname =
  [| "--intf=rc"; "v4l://"; "--vout=image"; "--image-out-replace";
     "--image-out-format=png"; "--image-out-prefix=" ^ fname |]

ELSE

(* Unix
   ----------------------------------------------------------------------

   At the beginning "gqcam --dump" was expected to deliver webcam
   snapshots.  However when using this command line snapshot
   facility, the colors did not come out well.

   We then tried vlc using

   * the rc or telnet interface but there is currently no way of
   requesting a snapshot through these interfaces -- it should be
   available in future version according to
   http://forum.videolan.org/viewtopic.php?f=7&t=16492. (vlc
   --intf=rc v4l:// --snapshot-path /tmp/ --snapshot-prefix ocaml
   --snapshot-format png)

   * the continuous file saving mode (along the lines of "vlc
   --intf=rc v4l:// --vout=image --image-out-replace
   --image-out-format=png --image-out-prefix=rubik") but no color
   correction is the applied

   Finally, we tried the simple 'webcam' program (from xawtv, Debian
   package: webcam) to grab the frames but it simply does not work...

   => We use vlc for its potential future...
*)

let vlc = "vlc"
let vlc_args fname =
  [| "--intf=rc"; "v4l://"; "--vout=image"; "--image-out-replace";
     "--image-out-format=png"; "--image-out-prefix=" ^ fname |]

ENDIF
ENDIF

type webcam = {
  pid : int;
  png : string;
}

let start () =
  let fname = Filename.temp_file "rubik" "" in
  (* FIXME: If one connects a pipe to input commands to vlc, vlc does
     not work as expected.  With [Unix.open_process_in],
     [Unix.close_process_in] does not terminate the process.  So we
     use the pid and [kill]. *)
  IFDEF WIN32 THEN (
    let pid =
      Unix.create_process vlc (vlc_args fname)
        Unix.stdin Unix.stdout Unix.stderr in
    { pid = pid;  png = fname ^ ".png" }
  )
  ELSE (
    match Unix.fork() with
    | 0 -> Unix.execv vlc (vlc_args fname)
    | pid -> { pid = pid;  png = fname ^ ".png" }
  )
  ENDIF
;;

let stop w =
  Unix.kill w.pid Sys.sigkill;
  Unix.unlink w.png
;;



let exchange_rb img =
  let ret = Array.make_matrix (Array.length img) (Array.length img.(0)) red in
  let rgb_components c =
    (c lsr 16) land 0xFF,
    (c lsr 8) land 0xFF,
    c land 0xFF in
  for i = 0 to (Array.length img) - 1
  do
    for j = 0 to (Array.length img.(0)) -1
    do
      let (r,g,b) = rgb_components img.(i).(j) in
      ret.(i).(j) <- Graphics.rgb b g r
    done;
  done;
  ret

let take w =
  wait_for_file w.png;
  let png = Filename.temp_file "rubik_" ".png" in
  copy w.png png;
  let ppm = Filename.temp_file "rubik_" ".ppm" in
  convert png ppm;
  let img = exchange_rb (Ppm.as_matrix_exn ppm) in
  Unix.unlink png;
  Unix.unlink ppm;
  img



(* Local Variables: *)
(* compile-command: "make -k snapshot.cmo" *)
(* End: *)
