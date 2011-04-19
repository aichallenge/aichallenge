type ant = { loc : int * int; owner : int; }
type game_setup = {
  loadtime : int;
  turntime : int;
  rows : int;
  cols : int;
  turns : int;
  viewradius2 : int;
  attackradius2 : int;
  spawnradius2 : int;
}
type mapb = { content : int; seen : int; }
type tgame_state = {
  setup : game_setup;
  turn : int;
  my_ants : ant list;
  enemy_ants : ant list;
  dead_ants : ant list;
  food : (int * int) list;
  tmap : mapb array array;
  go_time : float;
}
type dir = [ `E | `N | `S | `Stop | `W ]
type tile = [ `Dead | `Food | `Land | `Unseen | `Water ]
type order = (int * int) * dir

val ddebug : string -> unit
val issue_order : (int * int) * [< `E | `N | `S | `Stop | `W ] -> unit
val finish_turn : unit -> unit
val step_dir :
  [< `E | `N | `S | `Stop | `W ] -> int * int -> int * int -> int * int
val get_tile :
  mapb array array ->
  int * int -> [> `Ant | `Dead | `Food | `Land | `Unseen | `Water ]
val direction :
  int * int ->
  int * int -> int * int -> [> `N | `S | `Stop ] * [> `E | `Stop | `W ]
val distance2 : int * int -> int * int -> int * int -> int
val distance : int * int -> int * int -> int * int -> float
val distance_and_direction :
  int * int ->
  int * int ->
  int * int -> ([> `N | `S | `Stop ] * [> `E | `Stop | `W ]) * float
val update_vision : ant list -> tgame_state -> unit
val visible : tgame_state -> int * int -> bool
val passable : tgame_state -> int * int -> bool
val centre : tgame_state -> int * int
val time_remaining : tgame_state -> float
val loop : (tgame_state -> 'a) -> unit
