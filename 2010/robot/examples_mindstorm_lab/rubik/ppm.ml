(* File: ppm.ml

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

(* This library has benn written -- instead of using camlimages -- for
   windows portability. *)

(** Buffered read module with functions tailored to our needs *)
module Read =
struct
  (* Specialize [min] to integers for performance reasons (> 150% faster). *)
  let min x y = if (x:int) <= y then x else y

  let buffer_len = 4096

  type channel = {
    in_ch : in_channel;
    in_buf : string;   (* The data in the in_buf is at indexes i s.t. *)
    mutable in0 : int; (* in0 <= i < in1. *)
    mutable in1 : int; (* Invariant: 0 <= in0 ; in1 <= buffer_len
                          in1 < 0 indicates a closed channel. *)
  }

  let open_in fname = {
    in_ch = open_in_bin fname;
    in_buf = String.create buffer_len; in0 = 0; in1 = 0 }

  let close_in chan =
    if chan.in1 >= 0 then begin
      (* [chan] not yet closed *)
      close_in chan.in_ch;
      chan.in0 <- 0;
      chan.in1 <- -1
    end

  (* [fill_in_buf chan] refills in_buf if needed (when empty).  After
     this [in0 < in1] or [in1 = 0], the latter indicating that the end
     of file is reached (and then in0 = 0). *)
  let fill_in_buf chan =
    if chan.in0 >= chan.in1 then begin
      chan.in0 <- 0;
      chan.in1 <- input chan.in_ch chan.in_buf 0 buffer_len;
    end

  (* Reads a single byte. *)
  let byte chan =
    fill_in_buf chan;
    if chan.in1 = 0 then raise End_of_file else begin
      let c = chan.in_buf.[chan.in0] in
      chan.in0 <- chan.in0 + 1;
      Char.code c
    end

  let unsafe_input chan buf ofs len =
    fill_in_buf chan;
    let r = min len (chan.in1 - chan.in0) in
    String.blit chan.in_buf chan.in0 buf ofs r;
    chan.in0 <- chan.in0 + r;
    r

  let rec fill_string chan s ofs len =
    if len > 0 then
      let r = unsafe_input chan s ofs len in
      if r = 0 then String.sub s 0 ofs
      else fill_string chan s (ofs+r) (len-r)
    else s

  (** Reads a string of [n] characters.  If there are less than [n]
      chars left in the channel, return these. *)
  let string chan len = fill_string chan (String.create len) 0 len

  let is_space c =
    c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\011' || c = '\012'

  (** Skips whithespace.  Also, if we encounter a '#', skip till the
      end of the line.  If the end of file is reached, do nothing. *)
  let rec skip_spaces_and_comments chan =
    fill_in_buf chan;
    if chan.in1 > 0 (* not End_of_file *) then begin
      let c = chan.in_buf.[chan.in0] in
      if is_space c then begin
        chan.in0 <- chan.in0 + 1;
        skip_spaces_and_comments chan
      end
      else if c = '#' then skip_till_eol chan
    end
  and skip_till_eol chan =
    fill_in_buf chan;
    if chan.in1 > 0 then begin
      let c = chan.in_buf.[chan.in0] in
      chan.in0 <- chan.in0 + 1;
      if c = '\n' || c = '\r' then skip_spaces_and_comments chan
      else skip_till_eol chan
    end

  let zero = Char.code '0'
  let rec gather_int chan n =
    fill_in_buf chan;
    if n < 0 then failwith "Ppm.uint: max_int exceeded";
    if chan.in1 = 0 then n              (* end of file *)
    else
      let c = chan.in_buf.[chan.in0] in
      if '0' <= c && c <= '9' then begin
        chan.in0 <- chan.in0 + 1;
        gather_int chan (n * 10 + Char.code c - zero)
      end
      else n                            (* next letter <> digit *)

  (** Reads an ASCII unsigned integer. *)
  let uint chan =
    fill_in_buf chan;
    if chan.in1 = 0 then raise End_of_file
    else
      let c = chan.in_buf.[chan.in0] in
      if c < '0' || c > '9' then failwith "Ppm.Read.uint"
      else gather_int chan 0

  (** Read a RGB color. *)
  let get_color maxval =
    assert(maxval >= 0);
    if maxval = 255 then (fun chan -> byte chan)
    else
      let fmaxval = float maxval in
      if maxval < 255 then
        (fun chan -> truncate(255. *. float(byte chan) /. fmaxval))
      else
        fun chan ->
          let c1 = byte chan in
          let c0 = byte chan in
          truncate(255. *. float((c1 lsl 8) lor c0) /. fmaxval)

  let get_color_ascii maxval =
    assert(maxval >= 0);
    if maxval = 255 then
      (fun chan -> skip_spaces_and_comments chan;  min 255 (uint chan))
    else
      let fmaxval = float maxval in
      fun chan ->
        skip_spaces_and_comments chan;
        let c = min maxval (uint chan) in
        truncate(255. *. float c /. fmaxval)
end

type color = int                        (* as Graphics *)
let transp = -1                         (* as Graphics *)

let gamma_transf g c =
  truncate(255. *. (float c /. 255.)**g)

(* TODO: several images in a file; P6 only (not a priority). *)
let as_matrix_exn ?(gamma=1.) ?(bgr=false) fname =
  let fh = Read.open_in fname in
  let magic = Read.string fh 2 in
  let ppm_ascii =
    if magic = "P6" then false
    else if magic = "P3" then true
    else failwith "Ppm.as_matrix_exn: not a PPM file (wrong magic number)" in
  Read.skip_spaces_and_comments fh;
  let width = Read.uint fh in
  if width > Sys.max_array_length then
    failwith "Ppm.as_matrix_exn: width exceeds Sys.max_array_length";
  Read.skip_spaces_and_comments fh;
  let height = Read.uint fh in
  if height > Sys.max_array_length then
    failwith "Ppm.as_matrix_exn: height exceeds Sys.max_array_length";
  Read.skip_spaces_and_comments fh;
  let maxval = Read.uint fh in
  let img = Array.create_matrix height width transp in
  let get_color =
    if ppm_ascii then Read.get_color_ascii maxval else Read.get_color maxval in
  let gamma_transf =
    if gamma = 1. then (fun x -> x) (* optimize *) else gamma_transf gamma in
  let rgb =
    if bgr then (fun r g b -> (b lsl 16) lor (g lsl 8) lor r)
    else (fun r g b -> (r lsl 16) lor (g lsl 8) lor b) in
  if ppm_ascii then Read.skip_spaces_and_comments fh (* be lenient *)
  else ignore(Read.string fh 1);             (* single white space *)
  for h = 0 to height - 1 do
    let row = img.(h) in
    for w = 0 to width - 1 do
      let r = gamma_transf (get_color fh) in (* or End_of_file *)
      let g = gamma_transf (get_color fh) in
      let b = gamma_transf (get_color fh) in
      row.(w) <- rgb r g b
    done;
  done;
  Read.close_in fh;
  img
