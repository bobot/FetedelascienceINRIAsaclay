(* File: snapshot.mli

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

(** This module provide a platform independent abstraction above the
    ways of taking a snapshot of a webcam on various platforms.

    It requires the programs {{:http://www.videolan.org/vlc/}vlc} and
    {{:http://www.imagemagick.org/}ImageMagick}. *)


type color = int                        (** color, compatible with Graphics *)

type webcam
  (** Represent a connected webcam. *)

val start : unit -> webcam
  (** Start the webcam and returns a handle to it.   You should make *)

val stop : webcam -> unit
  (** Stops the webcam. *)

val exchange_rb : color array array -> color array array
  (** Exange the red and blue of the rgb component of the color. *)

val take : webcam -> color array array
  (** Take a snapshot from the webcam and return the image. *)
