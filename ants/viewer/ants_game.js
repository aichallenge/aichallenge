var AntsGame = Object.create(Game);

AntsGame.UNSEEN_CC = '?'.charCodeAt(0);
AntsGame.WATER_CC = '%'.charCodeAt(0);
AntsGame.FOOD_CC = '*'.charCodeAt(0);
AntsGame.LAND_CC = '.'.charCodeAt(0);
AntsGame.DEAD_CC = '!'.charCodeAt(0);
AntsGame.ANT_CC = 'a'.charCodeAt(0);
AntsGame.LAST_ANT_CC = 'z'.charCodeAt(0);

AntsGame.init = function (options) {
    this.parse_map(options.map);
}

AntsGame.load_map = function (map) {
    var map_row = 0;
    var lines = map.split(/[\r\n]+/);
    
    // visualizer data
    this.turn = 0;
    this.max_turns = 0;
    this.map = [];
    this.num_players = 0;
    
    for (var l = 0, llen = lines.length; l < llen; ++l) {
        var tokens = lines[l].split(' ');
        if (tokens[0] === 'rows') {
            this.rows = parseInt(tokens[1]);
        } else if (tokens[0] === 'cols') {
            this.cols = parseInt(tokens[1]);
        } else if (tokens[0] === 'm') {
            var line = tokens[1];
            this.map.push([]);
            var map_col = 0;
            for (var c = 0, clen = line.length; c < clen; ++c) {
                cc = line.charCodeAt(c);
                if (cc === this.LAND_CC) {
                    this.map[map_row].push(this.LAND);
                } else if (cc === this.WATER_CC) {
                    this.map[map_row].push(this.WATER);
                } else if (cc === this.FOOD_CC) {
                    this.map[map_row].push(this.FOOD)
                } else if (cc >= this.ANT_CC && cc <= this.LAST_ANT_CC) {
                    this.map[map_row].push(cc - this.ANT_CC);
                } else {
                    throw { name: 'MapParseError',
                             message: 'Invalid character ' + line[c] };
                }
                map_col++;
            }
            map_row++;
        }
    }
}

AntsGame.start_game = function () {
    this.do_food(Math.floor(self.land_area/100));
}

AntsGame.start_turn = function () {
    this.turn += 1;
    this.killed_ants = [];
    for (var a = 0, alen = this.current_ants.length; a < alen; ++a) {
        var ant = this.current_ants[a];
        ant.moved = false;
    }
    this.revealed_water = [];
    for (var p = 0; p < this.num_players; ++p) {
        this.revealed_water.push([]);
    }
}

AntsGame.finish_turn = function () {
    this.resolve_orders();
    this.do_attack();
    this.do_spawn();
    this.do_food();
    
    for (var p = 0; p < this.num_players; ++p) {
        this.score_history[p].push(this.score[p]);
    }
    
    this.vision = [];
    for (var p = 0; p < this.num_players; ++p) {
        this.vision.push(this.get_vision(p));
    }
    this.update_revealed();
}

AntsGame.finish_game = function () {
}

AntsGame.kill_player = function (player) {
    this.killed[player] = true;
}

AntsGame.is_alive = function (player) {
    if (this.killed[player]) {
        return false;
    } else {
        return this.player_ants(player).length > 0;
    }
}

AntsGame.game_over = function () {
    var bots_alive = 0;
    for (var p = 0; p < this.num_players; ++p) {
        if (this.is_alive(player)) {
            bots_alive += 1;
        }
    }
    return bots_alive <= 1;
}

AntsGame.get_state = function () {
    var updates = this.get_state_updates();
    var state = '';
    for (var u = 0, ulen = updates.length; u < ulen; ++u) {
        state += updates[u].join(' ') + '\n';
    }
    return state;
}

AntsGame.get_player_start = function (player) {
    var start = 'turn ' + this.turn + '\n';
    start += 'loadtime ' + this.loadtime + '\n';
    start += 'turntime ' + this.turntime + '\n';
    start += 'rows ' + this.rows + '\n';
    start += 'cols ' + this.cols + '\n';
    start += 'turns ' + this.turns + '\n';
    start += 'viewradius2 ' + this.viewradius + '\n';
    start += 'attackradius2 ' + this.attackradius + '\n';
    start += 'spawnradius2 ' + this.spawnradius + '\n';
    if (this.seed !== null) {
        start += 'seed ' + this.seed + '\n';
    }
    if (typeof player === 'undefined') {
        start += this.render_map();
    }
    return start;
}

AntsGame.get_player_state = function (player) {
    return this.render_changes(player);
}

AntsGame.do_moves = function (player, moves) {
    var parsed = this.parse_orders(player, moves);
    var orders = parsed[0];
    var valid = parsed[1];
    var invalid = parsed[2];
    if (invalid.length === 0) {
        this.do_orders(player, orders);
    } else {
        this.kill_player(player);
    }
    return [valid, invalid];
}

AntsGame.get_scores = function () {
    
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

var Ant = {
    loc: null,
    owner: null,
    initial_loc: null,
    spawn_turn: null,
    prev_loc: null,
    killed: false,
    moved: false,
    die_turn: null,
    orders: []
};

Ant.create = function (loc, owner, spawn_turn) {
    var ant = Object.create(Ant);
    ant.loc = loc;
    ant.owner = owner;
    
    ant.initial_loc = loc;
    ant.spawn_turn = spawn_turn;
    
    return ant;
};

Ant.move = function (loc, direction) {
    direction = direction || '-';
    if (this.moved) {
        throw { name: "MoveAntError",
                 message: "This ant was already moved from " + this.prev_loc +
                          " to " + this.loc };
    }
    this.prev_loc = this.loc;
    this.loc = loc;
    this.moved = true;
    this.orders.push(direction);
};

var Food = {
    loc: null,
    start_turn: null,
    end_turn: null,
    ant: null
}

Food.create = function (loc, start_turn) {
    this.loc = loc;
    this.start_turn = start_turn;
}