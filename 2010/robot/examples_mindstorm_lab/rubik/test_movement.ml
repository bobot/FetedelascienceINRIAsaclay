(* File: test_RS.ml *)

open Printf
module Motor = Mindstorm.Motor

let conn = let bt =
  if Array.length Sys.argv < 2 then (
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1) in Mindstorm.connect_bluetooth bt

module C =
  struct
    let conn = conn
    let motor_fighter = Motor.a
    let motor_hand = Motor.b
    let motor_pf = Motor.c
    let push_hand_port = `S2
    let push_fighter_port = `S1
    let cog_is_set_left = true
  end

module M = Movement.Make(C)

let()=
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> M.end_cont()));
  printf "Press Ctrl-c to quit.\n%!";
  M.kick(
    fun _ -> M.turn_pf 1(
      fun _-> M.turn_rubik_right(
        fun _ -> M.kick(
          fun _ -> M.turn_rubik_left(
            fun _ -> M.kick(
              fun _ -> M.turn_pf (-1) (
                fun _ -> M.kick(
                  fun _ -> M.turn_rubik_half(
                    fun _ -> M.kick(
                      fun _ -> M.turn_pf (-1)(
                        fun _ -> M.end_cont())))))))))));
  M.execute()
