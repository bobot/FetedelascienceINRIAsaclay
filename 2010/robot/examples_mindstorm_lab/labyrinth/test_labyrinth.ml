(* To be executed in the toploop *)
#load "graphics.cma";;
#load "labyrinth.cmo";;
#load "display.cmo"

open Printf;;
module Labyrinth = Display.Make(Labyrinth);;
open Labyrinth;;

(* Run *)
set_wall `Front false;
set_wall `Left false;
set_wall `Right true;
(* Try with and without the following: *)
set_wall `Back false;
move();;

set_wall `Front true;
set_wall `Left false;
set_wall `Right false;
turn `Left;
move();;

set_wall `Front false;
set_wall `Left false;
set_wall `Right true;
move();;

set_wall `Front true;
set_wall `Left false;
set_wall `Right true;
turn `Left;
move();;

set_wall `Front false;
set_wall `Right true;
set_wall `Left true;
move();;

set_wall `Front false;
set_wall `Right true;
set_wall `Left true;
move();;

set_wall `Front true;
set_wall `Left false;
set_wall `Right true;
turn `Left;
move();;

set_wall `Front false;
set_wall `Left false;
set_wall `Right true;
move();;

set_wall `Front false;
set_wall `Left false;
set_wall `Right true;
move();;

set_wall `Front false;
set_wall `Left false;
set_wall `Right true;
turn `Left;
move();;

set_wall `Front true;
set_wall `Left false;
set_wall `Right false;
turn `Right;
move();;

set_wall `Front true;
set_wall `Left false;
set_wall `Right true;
turn `Left;
move();;

set_wall `Front false;
set_wall `Left true;
set_wall `Right true;
move();;

set_wall `Front true;
set_wall `Left false;
set_wall `Right true;
turn `Left;
move();;

(* set_wall `Front true; *)
set_wall `Left false;
set_wall `Right true;
;;

(* Functions of solver.ml to declare (from CoordAndPath to path_to_closer) *)
let is_x_roads (sq,_) = Labyrinth.status sq = `Cross_roads;;
path_to_closer (Labyrinth.robot_pos()) is_x_roads;;

turn `Back; move();;
turn `Left; move(); move();;

path_to_closer (Labyrinth.robot_pos()) is_x_roads;;
