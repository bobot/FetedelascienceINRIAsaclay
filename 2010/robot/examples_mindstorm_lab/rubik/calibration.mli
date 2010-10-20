(** This module is used to calibrate the solver *)

val calibrate_webcam : ?snapshot_file:string -> unit -> unit
(** [calibrate_webcam ?snapshot_file _] hepls for putting the picking color
    area on good position on the snapshot. [snapshot_file] is the way to
    access to she ppm snapshot file.*)

(** To calibrate the mechanics *)
val calibrate_mechanics : unit -> unit
