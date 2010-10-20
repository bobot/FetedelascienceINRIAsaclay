(* File: translator.ml

   Copyright (C) 2008

     Dany Maslowski <dan_86@users.sourceforge.net>

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)



open Rubik
open Mindstorm

module type TranslatorT =
sig

  val show_cube : unit -> unit

  val make : Rubik.generator * int -> unit

  val face_iter : (Rubik.generator -> int -> unit) -> unit

  val return_face_init : unit -> unit

end

module Make (C: sig
               val conn : Mindstorm.bluetooth Mindstorm.conn
               val motor_fighter : Motor.port
               val motor_hand : Mindstorm.Motor.port
               val motor_pf : Motor.port
               val push_hand_port : Sensor.port
               val push_fighter_port : Sensor.port
               val cog_is_set_left : bool
             end) =

struct
  open C

  module M = Movement.Make(C)

  let show_cube () =
    let music () = Sound.play conn "! Startup.rso" in
    M.free_rubik (fun _ -> music(); M.turn_pf 4 M.end_cont);
    M.execute()

  (** Each face of the cube is represented by a letter (F,R,B,L,U,D)
      This fonction associate an integer between 0 and 5 to this letters,
      which represent the clue of the array state.
      Noted that letters assiated to faces are invariant by the mouvement
      of the machine. *)
  let transform_gen generator =
    match generator with
      F -> 0
    | R -> 1
    | B -> 2
    | L -> 3
    | U -> 4
    | D -> 5

(** For each clues (represantind letters of faces), we associate an integer
    between 0 and 5, which represent it position relative to the fighter.
    There are the front position repesented by 0, the right postion reprented
    by 1, back by 2, left by 3, up by 4 and down by 5. So, we have:
    t[0] = position relative to the fighter of the face F
    t[1] = position relative to the fighter the face R
    t[2] = position relative to the fighter the face B
    t[3] = position relative to the fighter the face R
    t[4] = position relative to the fighter the face U
    t[5] = position relative to the fighter the face D *)
  let state = [|0;1;2;3;4;5|]

  (** This table represent the permutation of the cube after a kick.
      permut_kick[i]=j say that the face who was in position i relative to
      the fighter before the kick will be in position j relative to the
      fighter after the kick. *)
  let permut_kick = [|4;1;5;3;2;0|]

  (** This table represent the permutation of the cube after a kick. *)
  let permut_rot = [|3;0;1;2;4;5|]

  (** Update the state of the cube dependent of the permution associated to
      one movement. *)
  let update_state permut_tab nbr =
    for i = 1 to nbr do
      state.(0) <- permut_tab.(state.(0));
      state.(1) <- permut_tab.(state.(1));
      state.(2) <- permut_tab.(state.(2));
      state.(3) <- permut_tab.(state.(3));
      state.(4) <- permut_tab.(state.(4));
      state.(5) <- permut_tab.(state.(5))
    done

  let rotate nbr_rot =
    match nbr_rot with
      0 -> M.end_cont()
    | 1 -> M.turn_rubik_right (~cont:M.end_cont)
    | 2 -> M.turn_rubik_half (~cont:M.end_cont)
    | _ -> M.turn_rubik_left (~cont:M.end_cont)

  (** Create the physical movement associeted to one (generator*int),
      which represent a movement. *)
  let make (generator, r) =
    begin
      match state.(transform_gen generator) with
        0 ->
          update_state permut_kick 3;
          M.kick(fun _ -> M.kick(fun _  -> M.kick(fun _ -> rotate r)))
      | 1 ->
          update_state permut_rot 3;
          update_state permut_kick 1;
          M.turn_pf (-1) (fun _ -> M.kick(fun _ -> rotate r))
      | 2 ->
          update_state permut_kick 1;
          M.kick(fun _ -> rotate r)
      | 3 ->
          update_state permut_rot 1;
          update_state permut_kick 1;
          M.turn_pf 1 (fun _ -> M.kick(fun _ -> rotate r))
      | 4 ->
          update_state permut_kick 2;
          M.kick(fun _ -> M.kick(fun _ -> rotate r))
      | _ ->
          rotate r
    end;
    M.execute()

  let face_iter f =
    M.free_rubik(
      fun _ -> f L 3; M.kick(
        fun _ -> M.free_rubik(
          fun _ -> f B 3; M.kick(
            fun _ -> M.free_rubik(
              fun _ -> f R 3; M.kick(
                fun _ -> M.free_rubik(
                  fun _ -> f F 3; M.turn_pf 1(
                    fun _ -> M.kick(
                      fun _ -> M.free_rubik(
                        fun _ -> f D 0; M.kick(
                          fun _ -> M.kick(
                            fun _ -> M.free_rubik(
                              fun _ -> f U 0; M.end_cont())))))))))))));
    M.execute()


  let return_face_init () =
    M.free_rubik( fun _ ->
      M.turn_pf (3) ( fun _ ->
        M.kick( fun _ ->
          M.free_rubik( fun _ ->
            M.turn_pf (3) (fun _ -> M.end_cont())))));
    M.execute()

end
