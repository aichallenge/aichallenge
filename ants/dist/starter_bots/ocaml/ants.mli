val ddebug : string -> unit
type game_setup = {
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
type mapb = { content : int; seen : int; }
class ant :
  row:int ->
  col:int ->
  owner:int ->
  object
    method col : int
    method loc : int * int
    method owner : int
    method row : int
    method to_string : string
  end
type tgame_state = {
  setup : game_setup;
  turn : int;
  my_ants : ant list;
  enemy_ants : ant list;
  my_hills : ((int * int) * int) list;
  enemy_hills : ((int * int) * int) list;
  dead_ants : ant list;
  food : (int * int) list;
  tmap : mapb array array;
  go_time : float;
}
type dir = [ `E | `N | `S | `Stop | `W ]
type tile = [ `Ant | `Dead | `Food | `Land | `Unseen | `Water ]
type order = (int * int) * dir
class swrap :
  tgame_state ->
  object
    val mutable state : tgame_state
    method bounds : int * int
    method centre : int * int
    method direction : int * int -> int * int -> dir * dir
    method distance : int * int -> int * int -> float
    method distance2 : int * int -> int * int -> int
    method distance_and_direction :
      int * int -> int * int -> (dir * dir) * float
    method finish_turn : unit -> unit
    method get_map : mapb array array
    method get_state : tgame_state
    method get_tile : int * int -> tile
    method issue_order : order -> unit
    method my_ants : ant list
    method enemy_ants : ant list
    method my_hills : ((int * int) * int) list
    method enemy_hills : ((int * int) * int) list
    method passable : int * int -> bool
    method set_state : tgame_state -> unit
    method step_dir : int * int -> dir -> int * int
    method time_remaining : float
    method turn : int
    method update_vision : unit
    method visible : int * int -> bool
    method get_player_seed : int
  end

val loop : (swrap -> 'a) -> unit
