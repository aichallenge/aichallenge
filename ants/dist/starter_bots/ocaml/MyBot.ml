open Ants;;

(* Since using a proper passable function would be incorrect for the 
starter bot, this not_water function will be used instead. Note the use 
of the get_tile function. *)

let not_water state loc =
   not ((get_tile state.tmap loc) = `Water)
;;

(* step_ant makes use of the step_dir function to test all of the 
options in order, and take the first one found; otherwise, does 
nothing. *)

let rec try_steps state ant bounds dirs =
   match dirs with [] -> ()
   | d :: tail ->
        if not_water state (step_dir d bounds ant.loc) then
           issue_order (ant.loc, d)
        else try_steps state ant bounds tail
;;

let step_ant state ant =
   let bounds = (state.setup.rows, state.setup.cols) in
      try_steps state ant bounds [`N; `E; `S; `W]
;;

(* This steps through a list of ants using tail recursion and attempts 
to order all of them to move. *)

let rec step_ants state my_l =
   match my_l with
    | [] -> ()
    | head :: tail ->
         step_ant state head;
         step_ants state tail
;;

(* 
The bot checks whether it's Turn 0 (setting up turn, no orders 
allowed) and finishes the turn immediately if it is; otherwise it calls 
step_ants. 

Referencing the map directly rather than through get_tile will yield an 
int * int , not a tile type. There is a tile_of_int function which can 
convert the first int to its tile type, such as `Water. See Ants.mli for 
more tile types, and see README.md or Ants.ml for the map format. 
*)

let mybot_engine state =
   if state.turn = 0 then finish_turn ()
   else
    (
(* This update_vision function is optional; with a lot of ants, it could 
chew up a fair bit of processor time. If you don't call it, the visible 
function won't work.
      update_vision state.my_ants state;
*)
      step_ants state state.my_ants;
      ddebug (Printf.sprintf "Time remaining: %f\n"
         (time_remaining state));
      finish_turn ()
    )
;;

loop mybot_engine;;
