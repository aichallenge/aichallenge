//Games used by the engine should implement the following methods

var Game = {};

Game.init = function (options) {
    throw { name: "NotImplemented",
             message: "Game.init(options) not implemented yet." };
}

// load starting map or game board positions
Game.load_map = function (map_text) {
    throw { name: "NotImplemented",
             message: "Game.load_map(map_text) not implemented yet." };
}

// do things needed to start a new game (init vars, etc...)
Game.start_game = function () {
    throw { name: "NotImplemented",
             message: "Game.start_game() not implemented yet." };
}

// do things needed for start of turn (cleanup, etc...)
Game.start_turn = function () {
    throw { name: "NotImplemented",
             message: "Game.start_turn() not implemented yet." };
}


// do things needed for finishing a turn (resolving orders, scoring, etc...)
Game.finish_turn = function () {
    throw { name: "NotImplemented",
             message: "Game.finish_turn() not implemented yet." };
}

// do things needed for finishing a game (scoring, etc...)
Game.finish_game = function () {
    throw { name: "NotImplemented",
             message: "Game.finish_game() not implemented yet." };
}

// remove a player from the game, may be a crashed/timed out bot
Game.kill_player = function (player) {
    throw { name: "NotImplemented",
             message: "Game.kill_player(player) not implemented yet." };
}

// return if a player is alive, might be removed by game mechanics
Game.is_alive = function (player) {
    throw { name: "NotImplemented",
             message: "Game.is_alive(player) not implemented yet." };
}

// returns if the game is over due to a win condition
Game.game_over = function () {
    throw { name: "NotImplemented",
             message: "Game.game_over() not implemented yet." };
}

// used by engine to get the current game state for the streaming format
Game.get_state = function () {
    throw { name: "NotImplemented",
             message: "Game.get_state not implemented yet." };
}

// used for turn 0, sending minimal info for bot to load
// when player is undefined, the output is used at the start of the streaming
//   format
Game.get_player_start = function (player) {
    throw { name: "NotImplemented",
             message: "Game.get_player_start(player) not implemented yet." };
}

// used for sending state to bots for each turn
Game.get_player_state = function (player) {
    throw { name: "NotImplemented",
             message: "Game.get_player_state(player) not implemented yet." };
}

// process a single player's moves, may be appropriate to resolve during finish turn
Game.do_moves = function (player, moves) {
    throw { name: "NotImplemented",
             message: "Game.do_moves(player, moves) not implemented yet." };
}

// will be an array of moves for each player
Game.do_all_moves = function (all_moves) {
    for (var i = 0, len = all_moves.length; i < len; ++i) {
        this.do_moves(i, all_moves[i]);
    }
}

// used for ranking
Game.get_scores = function () {
    throw { name: "NotImplemented",
        message: "Game.get_scores() not implemented yet." };
}

// can be used to determine fairness of game and other stuff for visualizers
Game.get_stats = function () {
    throw { name: "NotImplemented",
        message: "Game.get_stats() not implemented yet." };
}
    
// used for getting a compact replay of the game
Game.get_replay = function () {
    throw { name: "NotImplemented",
        message: "Game.get_replay() not implemented yet." };
}
