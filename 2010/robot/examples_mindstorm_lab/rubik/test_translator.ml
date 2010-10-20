(* File: test_translator.ml *)

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

module M = Translator.Make(C)

let () = List.iter (M.make) ([(Rubik.L,2);(Rubik.R,1);(Rubik.U,3);(Rubik.L,1)])
