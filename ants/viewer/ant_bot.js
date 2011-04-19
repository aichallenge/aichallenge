var AntBot = Object.create(Bot);

AntBot.AIM = {'n': [-1, 0],
       'e': [0, 1],
       's': [1, 0],
       'w': [0, -1]}
AntBot.RIGHT = {'n': 'e',
         'e': 's',
         's': 'w',
         'w': 'n'}
AntBot.LEFT = {'n': 'w',
        'e': 'n',
        's': 'e',
        'w': 's'}
AntBot.BEHIND = {'n': 's',
          's': 'n',
          'e': 'w',
          'w': 'e'}

AntBot.UNSEEN = -5;
AntBot.WATER = -4;
AntBot.FOOD = -3;
AntBot.LAND = -2;
AntBot.DEAD = -1;
AntBot.ANT = 0;
AntBot.MY_ANT = 0;

AntBot.input = function (data) {
    this.orders = [];
    this.do_turn();
    this.finish_turn();
    return this.orders.join('');
}

AntBot.setup = function (data) {
    var lines = data.split(/[\r\n]+/);
    for (var l = 0, llen = lines.length; l < llen; ++l) {
        var tokens = lines[l].split(' ');
        if (tokens[0] === 'cols') {
            this.cols = parseInt(tokens[1]);
        } else if (tokens[0] === 'rows') {
            this.rows = parseInt(tokens[1]);
        } else if (tokens[0] === 'seed') {
            this.seed = parseInt(tokens[1]);
        }
    }
    this.map = [];
    for (var row = 0; row < this.rows; ++row) {
        this.map.push([]);
        for (var col = 0; col < this.cols; ++col) {
            this.map[row].push(this.LAND);
        }
    }
    this.ant_list = [];
    this.food_list = [];
    this.dead_list = [];
}

AntBot.update = function (data) {
    // clear ant and food data
    for (var a = 0, alen = this.ant_list.len; a < alen; ++a) {
        var loc = this.ant_list[a][0];
        this.map[loc[0]][loc[1]] = this.LAND;
    }
    this.ant_list = [];
    for (var f = 0, flen = this.food_list.len; f < flen; ++f) {
        var loc = this.food_list[f];
        this.map[loc[0]][loc[1]] = this.LAND;
    }
    this.food_list = [];
    for (var d = 0, dlen = this.dead_list.len; d < dlen; ++d) {
        var loc = this.dead_list[d];
        this.map[loc[0]][loc[1]] = this.LAND;
    }
    this.dead_list = [];
    
    // update map and create new ant and food lists
    var lines = data.split(/[\r\n]+/);    
    for (var l = 0, llen = lines.length; l < llen; ++l) {
        var tokens = lines[l].split(' ');
        var row = parseInt(tokens[1]);
        var col = parseInt(tokens[2]);
        if (tokens[0] === 'a') {
            var owner = parseInt(tokens[3]);
            this.map[row][col] = owner;
            this.ant_list.push([[row, col], owner]);
        } else if (tokens[0] === 'f') {
            this.map[row][col] = this.FOOD;
            this.food_list.push([row, col]);
        } else if (tokens[0] === 'd') {
            this.map[row][col] = this.DEAD;
            this.dead_list.push([row, col]);
        } else if (tokens[0] === 'w') {
            this.map[row][col] = this.WATER;
        }
    }
}

AntBot.issue_order = function (order) {
    this.orders.push('o ' + order[0] + ' ' + order[1] + ' ' + order[2] + '\n');
}

AntBot.finish_turn = function () {
    this.orders.push('go\n');
}

AntBot.my_ants = function () {
    var my_ants = [];
    for (var a = 0, alen = this.ant_list.len; a < alen; ++a) {
        var loc = this.ant_list[a][0];
        if (this.ant_list[a][1] === this.MY_ANT) {
            my_ants.push(this.ant_list[a][0]);
        }
    }
    return my_ants;
}

AntBot.enemy_ants = function () {
    var enemy_ants = [];
    for (var a = 0, alen = this.ant_list.len; a < alen; ++a) {
        var loc = this.ant_list[a][0];
        if (this.ant_list[a][1] !== this.MY_ANT) {
            enemy_ants.push(this.ant_list[a][0]);
        }
    }
    return enemy_ants;
}

AntBot.food = function () {
    return this.food_list.slice();
}

AntBot.passable = function (loc) {
    return this.map[loc[0]][loc[1]] > this.WATER;
}

AntBot.unoccupied = function (loc) {
    return (this.map[loc[0]][loc[1]] === this.LAND ||
             this.map[loc[0]][loc[1]] === this.DEAD);
}

AntBot.destination = function (loc, direction) {
    d = this.AIM[direction];
    return [(loc[0] + d[0] + this.rows) % this.rows,
             (loc[1] + d[1] + this.cols) % this.cols];
}

AntBot.distance = function (loc_a, loc_b) {
    var d_row = Math.min(Math.abs(loc_a[0] - loc_b[0]),
                         this.rows - Math.abs(loc_a[0] - loc_b[0]));
    var d_col = Math.min(Math.abs(loc_a[1] - loc_b[1]),
                         this.cols - Math.abs(loc_a[1] - loc_b[1]));
    return d_row + d_col;
}

AntBot.direction = function (loc_a, loc_b) {
    d = [];
    var rows_half = Math.floor(this.rows / 2);
    var cols_half = Math.floor(this.cols / 2);
    if (loc_a[0] < loc_b[0]) {
        if (loc_b[0] - loc_a[0] >= rows_half) {
            d.push('n');
        }
        if (loc_b[0] - loc_a[0] <= rows_half) {
            d.push('s');
        }
    }
    if (loc_b[0] < loc_a[0]) {
        if (loc_a[0] - loc_b[0] >= rows_half) {
            d.push('s');
        }
        if (loc_a[0] - loc_b[0] <= rows_half) {
            d.push('n');
        }
    }
    if (loc_a[1] < loc_b[1]) {
        if (loc_b[1] - loc_a[1] >= cols_half) {
            d.push('w');
        }
        if (loc_b[1] - loc_a[1] <= cols_half) {
            d.push('e');
        }
    }
    if (loc_b[1] < loc_a[1]) {
        if (loc_a[1] - loc_b[1] >= cols_half) {
            d.push('e');
        }
        if (loc_a[1] - loc_b[1] <= cols_half) {
            d.push('w');
        }
    }
    return d;
}