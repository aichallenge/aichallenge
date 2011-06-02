open Ants;;

(* Since using the proper passable function would be incorrect for the 
starter bot, this not_water function will be used instead. Note the use 
of the get_tile function. *)

let not_water state loc =
   not ((state#get_tile loc) = `Water)
;;

(* note the use of step_dir *)
let rec try_steps state ant dirs =
   match dirs with [] -> ()
   | d :: tail ->
        if not_water state (state#step_dir ant#loc d) then
           state#issue_order (ant#loc, d)
        else try_steps state ant tail
;;

(* step_ant makes use of the try_steps function to test all of the 
options in order, and take the first one found; otherwise, does 
nothing. *)

let step_ant state ant =
   try_steps state ant [`N; `E; `S; `W]
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

(* The bot checks whether it's Turn 0 (setting up turn, no orders 
allowed) and finishes the turn immediately if it is; otherwise it calls 
step_ants. *)

(* The update_vision function is optional; with a lot of ants, it could 
chew up a fair bit of processor time. If you don't call it, the visible 
function won't work. *)

let mybot_engine state =
   if state#turn = 0 then state#finish_turn ()
   else
    (
      state#update_vision;
      step_ants state state#my_ants;
      state#finish_turn ()
    )
;;

loop mybot_engine;;
