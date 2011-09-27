(* ocaml Ants starter package. Code has been borrowed from the ocaml
PlanetWars starter package and adapted. If you find any bugs or make 
any improvements, please post to the forum or upload a fix! *)

let out_chan = stderr (* open_out "mybot_err.log" *);;

(* this previously used Sys.time, but it's wrong in this context
 *)
let get_time () = Unix.gettimeofday ();;

let ddebug s = 
   output_string out_chan s; 
   flush out_chan
;;

type game_setup =
 {
   loadtime : int;
   turntime : int;
   rows : int;
   cols : int;
   turns : int;
   viewradius2 : int;
   attackradius2 : int;
   spawnradius2 : int;
   player_seed : int;
 }
;;

type mapb = 
 {
   content : int;
   seen : int
 }
;;

(* You can change this to a record if you want to remove OO stuff *)

class ant ~row ~col ~owner =
   object
      method loc = row, col
      method row = row
      method col = col
      method owner = owner
      method to_string =
         Printf.sprintf "Ant at %d, %d belongs to player %d" 
            row col owner;
   end
;;

type tgame_state =
  { 
    setup : game_setup;
    turn : int;
    my_ants : ant list;
    enemy_ants : ant list;
    my_hills : ((int * int) * int) list;
    enemy_hills : ((int * int) * int) list;
    dead_ants : ant list;
    food : (int * int) list;
    tmap: mapb array array; 
    go_time: float;
 }
;;

type dir = [ `N | `E | `S | `W | `Stop];;

type tile = [ `Water | `Land | `Food | `Ant | `Dead | `Unseen];;

type order = ((int * int) * dir);;

let proto_tile =
 {
   content = 0;
   seen = 0;
 }
;;

let tile_of_int c =
   if c = 0 then `Unseen
   else if c = 1 then `Land
   else if c = 2 then `Water
   else if c = 3 then `Food
   else if (c > 99) && (c < 200) then `Ant
   else `Dead
;;

let string_of_dir d = 
   match d with
    | `N -> "N" 
    | `E -> "E"
    | `S -> "S"
    | `W -> "W"
    | `Stop -> "Stop"
;;

let int_of_tile t =
   match t with
    | `Unseen -> 0
    | `Land -> 1
    | `Water -> 2
    | `Food -> 3
    | `Ant -> 199
    | `Dead -> 299
;;

(* Begin input processing stuff *)

let set_turn gstate v =
   {gstate with turn = v}
;;

let set_loadtime gstate v =
   {gstate with setup = {gstate.setup with loadtime = v}}
;;

let set_turntime gstate v = 
   {gstate with setup = {gstate.setup with turntime = v}}
;;

let set_rows gstate v = 
   {gstate with setup = {gstate.setup with rows = v}}
;;

let set_cols gstate v = 
   {gstate with setup = {gstate.setup with cols = v}}
;;

let set_turns gstate v = 
   {gstate with setup = {gstate.setup with turns = v}}
;;

let set_viewradius2 gstate v = 
   {gstate with setup = {gstate.setup with viewradius2 = v}}
;;

let set_attackradius2 gstate v = 
   {gstate with setup = {gstate.setup with attackradius2 = v}}
;;

let set_spawnradius2 gstate v = 
   {gstate with setup = {gstate.setup with spawnradius2 = v}}
;;

let set_player_seed gstate v = 
   {gstate with setup = {gstate.setup with player_seed = v}}
;;

let uncomment s =
  try String.sub s 0 (String.index s '#')
  with Not_found -> s

let sscanf_cps fmt cont_ok cont_fail s =
  try Scanf.sscanf s fmt cont_ok
  with _ -> cont_fail s

let add_food gstate row col =
   gstate.tmap.(row).(col) <- 
      {gstate.tmap.(row).(col) with content = (int_of_tile `Food)};
   {gstate with food = ((row, col) :: gstate.food)}
;;

(*
let remove_food gstate row col =
   if gstate.tmap.(row).(col).content = (int_of_tile `Food) then
      gstate.tmap.(row).(col) <- 
         {gstate.tmap.(row).(col) with content = (int_of_tile `Land)};
   {gstate with food = (List.filter (fun p -> not (p = (row, col)))
                        gstate.food)}
;;
*)

let add_water gstate row col =
   gstate.tmap.(row).(col) <- 
      {gstate.tmap.(row).(col) with content = (int_of_tile `Water)};
   gstate
;;

(* Note that this clears previously seen food. *)

let clear_tile t =
   match (tile_of_int t.content) with
    | `Water | `Unseen -> t
    | _ -> {t with content = (int_of_tile `Land)}
;;

let clear_gstate gs =
 if gs.turn < 1 then gs else
  (
   for count_row = 0 to (Array.length gs.tmap - 1) do
      let test_row = gs.tmap.(count_row) in
      for count_col = 0 to (Array.length test_row - 1) do
         test_row.(count_col) <- clear_tile test_row.(count_col)
      done
   done;
   {gs with my_ants = []; enemy_ants = []; dead_ants = []; food = [];
         my_hills = []; enemy_hills = []}
  )
;;

let add_hill gstate row col owner =
   try
     (
      match owner with
       | 0 ->
            {gstate with my_hills = (((row, col), owner) :: gstate.my_hills)}
       | n ->
            {gstate with enemy_hills = 
               (((row, col), owner) :: gstate.enemy_hills)}
     )
   with _ -> gstate
;;

let add_ant gstate row col owner =
   try
     (
      gstate.tmap.(row).(col) <- 
         {gstate.tmap.(row).(col) with content = (100 + owner)};
      let new_ant = new ant row col owner in
      match owner with
       | 0 ->
            {gstate with my_ants = (new_ant :: gstate.my_ants)}
       | n ->
            {gstate with enemy_ants = (new_ant :: gstate.enemy_ants)}
     )
   with _ -> gstate
;;

let add_dead_ant gstate row col owner =
   try
     (
      gstate.tmap.(row).(col) <- 
         {gstate.tmap.(row).(col) with content = (200 + owner)};
      let new_ant = new ant row col owner in
      {gstate with dead_ants = (new_ant :: gstate.dead_ants)}
     )
   with _ -> gstate
;;

let initialize_map gstate =
   let new_map = 
      Array.make_matrix gstate.setup.rows gstate.setup.cols proto_tile
   in
   {gstate with tmap = new_map}
;;

(* This add_line function is a bit tricky to modify (make sure you get 
the parentheses in the right places if you change it). *)

let add_line gstate line =
   sscanf_cps "%s %d %d %d"
    (fun ad row col owner ->
       match ad with
        | "a" -> add_ant gstate row col owner
        | "d" -> add_dead_ant gstate row col owner
        | "h" -> add_hill gstate row col owner
        | bd -> gstate)
    (sscanf_cps "%s %d %d"
      (fun fw row col ->
         match fw with
          | "f" -> add_food gstate row col
          | "w" -> add_water gstate row col
(*
          | "r" -> remove_food gstate row col
*)
          | _ -> gstate)
      (sscanf_cps "%s %d"
        (fun key v ->
            match key with
             | "turn" -> set_turn gstate v
             | "loadtime" -> set_loadtime gstate v
             | "turntime" -> set_turntime gstate v
             | "rows" -> set_rows gstate v
             | "cols" -> set_cols gstate v
             | "turns" -> set_turns gstate v
             | "viewradius2" -> set_viewradius2 gstate v
             | "attackradius2" -> set_attackradius2 gstate v
             | "spawnradius2" -> set_spawnradius2 gstate v
             | "player_seed" -> set_player_seed gstate v
             | _ -> gstate
        )
        (fun (line : string) ->
          gstate
(* swap this for the above line if you want it to fail on bad input
          if line = "" then
            gstate
          else
            failwith (Printf.sprintf "unable to parse '%s'" line)
*)
        )))
    (uncomment line)

let update gstate lines =
   let cgstate =
      if gstate.turn = 0 then
         gstate
      else
         clear_gstate gstate
   in
   let ugstate =
      List.fold_left add_line cgstate lines 
   in if ugstate.turn = 0 then
      if ugstate.setup.rows < 0
      || ugstate.setup.cols < 0 then
        (
         ddebug "\nBad setup info! Expect crashes!\n\n";
         ugstate
        )
      else initialize_map ugstate
   else ugstate
;;

let read_lines () =
  let rec read_loop acc =
    let line = read_line () in
    if String.length line >= 2 && String.sub line 0 2 = "go" 
    || String.length line >= 3 && String.sub line 0 3 = "end"
    || String.length line >= 5 && String.sub line 0 5 = "ready" then
     (
      List.rev acc
     )
    else
      read_loop (line :: acc)
  in
  try Some (read_loop []) with End_of_file -> None
;;

let read gstate =
  let ll = read_lines () in
  let go_time = get_time () in
  match ll with
  | Some lines -> Some {(update gstate lines) with go_time = go_time}
  | None -> None
;;

(* End input section *)

(* Begin output section *)

let issue_order ((row, col), cdir) =
   let os = Printf.sprintf "o %d %d %s\n" row col (string_of_dir cdir)
   in
   Printf.printf "%s" os;
;;

(* Print go, newline, and flush buffer *)
let finish_turn () = Printf.printf "go\n%!";;

(* End output section *)

(* Helper functions *)

let step_unbound d (row, col) =
   match d with
    | `N -> (row - 1), col
    | `S -> (row + 1), col
    | `W -> row, (col - 1)
    | `E -> row, (col + 1)
    | `Stop -> row, col
;;

let rec wrap0 bound n =
   if bound < 0 then 
      (ddebug (Printf.sprintf "wrap0 below zero not allowed%!"); 0)
   else if n < 0 then wrap0 bound (n + bound)
   else if n >= bound then wrap0 bound (n - bound)
   else n
;;

let wrap_bound (rows, cols) (row, col) =
   wrap0 rows row, 
   wrap0 cols col
;;

(* tell me my target co-ordinates when I step in a direction *)
let step_dir d bounds (row, col) =
   let new_loc = step_unbound d (row, col) in
   wrap_bound bounds new_loc
;;

(* return the tile type at a location *)
let get_tile tmap (row, col) =
   try
      tile_of_int (tmap.(row).(col)).content
   with e -> ddebug (Printf.sprintf 
         "\nocaml Ants warning: exception getting tile %d, %d: %s\n" 
               row col (Printexc.to_string e));
         `Unseen
;;

(* shortest distance from point 1 to point 2: is it "inside", and how far? *)
let shorter_dist w p1 p2 =
   let d1 = abs (p2 - p1) in
   let d2 = w - d1 in
      (d1 < d2), (min d1 d2)
;;

(* see distance_and_direction below *)
let stepdistance_ndirection (rows, cols) (row1, col1) (row2, col2) =
   let row_si, row_dist =
      shorter_dist rows row1 row2
   in
   let col_si, col_dist =
      shorter_dist cols col1 col2
   in
   let row_dir =
     if row1 = row2 then `Stop
     else
      match row_si with true ->
         if row1 < row2 then `S
         else `N
      | false ->
         if row1 < row2 then `N
         else `S
   in
   let col_dir =
     if col1 = col2 then `Stop
     else
      match col_si with true ->
         if col1 < col2 then `E
         else `W
      | false ->
         if col1 < col2 then `W
         else `E
   in (row_dir, col_dir), (row_dist, col_dist)
;;

(* returns d, has a type declaration for some reason *)
let direction bounds p1 p2 =
   let d, _ = stepdistance_ndirection bounds p1 p2 in
      (d: (dir * dir)) 
;;

let fsquare_int i =
   let f = float_of_int i in f *. f
;;

(* distance squared *)
let distance2 (rows, cols) (src_row, src_col) (dst_row, dst_col) =
   let d1 = abs (src_row - dst_row) in
   let d2 = abs (src_col - dst_col) in
   let dr = min d1 (rows - d1) in
   let dc = min d2 (cols - d2) in
      (dr * dr) + (dc * dc)
;;

(* distance (not squared) *)
let distance b p1 p2 = sqrt (float_of_int (distance2 b p1 p2));;

(* returns the distance and the two directions you might travel in to 
get from p1 to p2 ignoring water, with `Stop(s) for none *)
let distance_and_direction bounds p1 p2 =
   let d, (r, c) = stepdistance_ndirection bounds p1 p2 in
      d, (sqrt ((fsquare_int r) +. (fsquare_int c)))
;;

let mark_seen turn (pr, pc) tmap =
   tmap.(pr).(pc) <- {tmap.(pr).(pc) with seen = turn}
;;

(* Draw a filled vision circle around an ant *)
let paint_fov ant gstate =
   let c_row, c_col = ant#loc in
   let bounds = gstate.setup.rows, gstate.setup.cols in
   let r2 = gstate.setup.viewradius2 in
   let r = int_of_float (sqrt (float_of_int r2)) in
   let ul_row, ul_col = wrap_bound bounds (c_row - r, c_col - r) in
   for count_rows = 0 to (r * 2) do
      for count_cols = 0 to (r * 2) do
         let pr, pc = 
            wrap_bound bounds (ul_row + count_rows, ul_col + count_cols)
         in
         if distance2 bounds (pr, pc) ant#loc <= r2 then
            mark_seen gstate.turn (pr, pc) gstate.tmap
      done
   done
;;

(* Update vision status for all ants *)
let rec update_vision my_ants gstate =
   match my_ants with
    | [] -> ()
    | head :: tail ->
         paint_fov head gstate;
         update_vision tail gstate
;;

(* Is tile at loc visible this turn? *)
let visible gstate (row, col) =
   gstate.tmap.(row).(col).seen = gstate.turn
;;

(* Is tile at loc passable (not blocked by water or food)? *)
let passable gstate (row, col) =
   let t = tile_of_int (gstate.tmap.(row).(col).content) in
      not ((t = `Water) || (t = `Food))
;;

let centre state = state.setup.rows / 2, state.setup.cols / 2;;

(* How many milliseconds remain? *)
let time_remaining state = 
   let turn_time = if state.turn = 0 then (float_of_int state.setup.loadtime)
   else (float_of_int state.setup.turntime) in
      1000. *. 
      ((turn_time /. 1000.) -. ((get_time ()) -. state.go_time))
;;

(* End helper functions *)

(* swrap wraps the game state. One of these is passed to the AI. *)

class swrap state =
 object (self)
   val mutable state = state
   method bounds = state.setup.rows, state.setup.cols
   method issue_order (o:order) = issue_order o
   method finish_turn () = finish_turn ()
   method direction p1 p2 = ((direction self#bounds p1 p2): (dir * dir))
   method step_dir loc (d:dir) = step_dir d self#bounds loc
   method get_tile loc = ((get_tile state.tmap loc): tile)
   method distance2 p1 p2 = distance2 self#bounds p1 p2
   method distance p1 p2 = distance self#bounds p1 p2
   method distance_and_direction p1 p2 =
      ((distance_and_direction self#bounds p1 p2): ((dir * dir) * float))
   method update_vision = update_vision state.my_ants state
   method visible loc = visible state loc
   method passable loc = passable state loc
   method centre = centre state
   method time_remaining = time_remaining state
   method set_state s = state <- s
   method get_state = state
   method turn = state.turn
   method my_ants = state.my_ants
   method enemy_ants = state.enemy_ants
   method my_hills = state.my_hills
   method enemy_hills = state.enemy_hills
   method get_map = state.tmap
   method get_player_seed = state.setup.player_seed
(*
   More getters to be added as needed
*)
 end
;;

(* Main game loop. Bots should define a main function taking a swrap for 
an argument (see above), and then call loop main_function. See how the 
starter bot in MyBot.ml does it if this doesn't make sense.
   This loop function will exit the program, killing your bot if an 
exception is raised. This is good for debugging, but should be changed 
before release. Calling the finish_turn function might be a good 
alternative. *)

let loop engine =
  let proto_setup =
     {
      loadtime = -1;
      turntime = -1;
      rows = -1;
      cols = -1;
      turns = -1;
      viewradius2 = -1;
      attackradius2 = -1;
      spawnradius2 = -1;
      player_seed = 932463947;
     }
  in
  let proto_gstate =
     {
      setup = proto_setup;
      turn = 0;
      my_ants = [];
      enemy_ants = [];
      my_hills = [];
      enemy_hills = [];
      dead_ants = [];
      food = [];
      tmap = Array.make_matrix 1 1 proto_tile; 
      go_time = 0.0;
     }
  in
  let wrap = new swrap proto_gstate in
  let rec take_turn i gstate =
    match read gstate with
    | Some state ->
        begin try 
         (
          wrap#set_state state;
          engine wrap;
          flush stdout;
         )
        with exc ->
         (
          ddebug (Printf.sprintf 
             "Exception in turn %d :\n" i);
          ddebug (Printexc.to_string exc);
          raise exc
         )
        end;
        take_turn (i + 1) wrap#get_state
    | None ->
        ()
  in
     take_turn 0 proto_gstate
;;
