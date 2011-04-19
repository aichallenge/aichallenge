var Ants = {};

// utility functions
Ants.parseQueryString = function () {
    var q = {};
    var pairs = location.search.substring(1).split('&');
    for (var i = 0, len = pairs.length; i < len; ++i) {
        var item = pairs[i].split('=');
        if (q[item[0]] === undefined) {
            q[item[0]] = item[1];
        } else {
            if (!isArray(q[item[0]])) {
                q[item[0]] = [q[item[0]]];
            }
            q[item[0]].push(item[1]);
        }
    }
    location.queryString = q;
}

Ants.zeroFill = function (number, width) {
    width -= number.toString().length;
    if (width > 0) {
        return new Array(width + (/\./.test(number) ? 2 : 1)).join('0') + number;
    }
    return number;
}

Ants.UNSEEN = -5;
Ants.WATER = -4;
Ants.FOOD = -3;
Ants.LAND = -2;
Ants.DEAD = -1;
Ants.ANT = 0;
Ants.UNSEEN_CC = '?'.charCodeAt(0);
Ants.WATER_CC = '%'.charCodeAt(0);
Ants.FOOD_CC = '*'.charCodeAt(0);
Ants.LAND_CC = '.'.charCodeAt(0);
Ants.DEAD_CC = '!'.charCodeAt(0);
Ants.ANT_CC = 'a'.charCodeAt(0);
Ants.LAST_ANT_CC = 'z'.charCodeAt(0);
Ants.COLOR = {"-5": "#000",
              "-4": "#115",
              "-3": "#eec",
              "-2": "#8b4513",
              "-1": "#693305",
              "0": "#f00",
              "1": "#ff0",
              "2": "#0f0",
              "3": "#00f"}

// options to pass:
//  data: replay data string
//  canvas: html canvas element
Ants.init = function (options) {
    this.parseData(options.data);
    this.canvas = options.canvas;
    this.dc = options.canvas.getContext("2d");
    this.dc.fillStyle = "#000";
    this.dc.fillRect(0, 0, this.canvas.width, this.canvas.height);
    this.x_scale = Math.min(Math.floor(this.canvas.width/this.width),
                            Math.floor(this.canvas.height/this.height));
    this.y_scale = this.x_scale;
    this.x_offset = this.width * this.x_scale / 2;
    this.y_offset = this.height * this.y_scale / 2;
}

Ants.parseData = function (data) {
    var turn = 0;
    var map_row = 0;
    var lines = data.split(/[\r\n]+/);
    
    // visualizer data
    this.turn = 0;
    this.max_turns = 0;
    this.width = 0;
    this.height = 0;
    this.map = [];
    this.changes = [];
    
    for (var l = 0, llen = lines.length; l < llen; ++l) {
        var tokens = lines[l].split(' ');
        if (tokens[0] === 'turn') {
            // assume all turns are in order and start at 0
            turn = parseInt(tokens[1]);
            this.changes.push([]);
        } else if (tokens[0] === 'end') {
            turn += 1;
            this.max_turns = turn;
            this.changes.push([]);
        } else if (tokens[0] === 'm') {
            if (this.changes.length === 0) {
                // used for map only mode
                this.changes.push([]);
            }
            // only except map input for turn 0
            if (turn === 0) {
                var line = tokens[1];
                this.width = line.length;
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
                        this.changes[turn].push([map_row, map_col, this.FOOD]);
                    } else if (cc >= this.ANT_CC && cc <= this.LAST_ANT_CC) {
                        this.map[map_row].push(cc - this.ANT_CC);
                        this.changes[turn].push([map_row, map_col, cc - this.ANT_CC]);
                    } else {
                        throw {name: 'MapParseError',
                               message: 'Invalid character ' + line[c]};
                    }
                    map_col++;
                }
                map_row++;
            }
        } else if (tokens[0] === 'a') {
            var row = parseInt(tokens[1]);
            var col = parseInt(tokens[2]);
            var owner = parseInt(tokens[3]);
            this.changes[turn].push([row, col, owner]);
        } else if (tokens[0] === 'f') {
            var row = parseInt(tokens[1]);
            var col = parseInt(tokens[2]);
            this.changes[turn].push([row, col, this.FOOD]);
        } else if (tokens[0] === 'd') {
            var row = parseInt(tokens[1]);
            var col = parseInt(tokens[2]);
            this.changes[turn].push([row, col, this.DEAD]);
        }
    }
    this.height = map_row;
}

Ants.viewTurn = function (turn) {
    //this.canvas.width = this.canvas.width;
    var x_offset = this.canvas.width / 2 - this.x_offset
    var y_offset = this.canvas.height / 2 - this.y_offset
    for (var row = 0; row < this.height; ++row) {
        for (var col = 0; col < this.width; ++col) {
            var ilk = this.map[row][col];
            // if not showing turn 0, then show only land and water
            // the call to showIlks will display the ants and food
            if (turn !== 0 && ilk !== this.WATER) {
                ilk = this.LAND;
            }
            this.dc.fillStyle = this.COLOR[ilk];
            var x = col * this.x_scale + x_offset;
            var y = row * this.y_scale + y_offset;
            this.dc.fillRect(x, y, this.x_scale, this.y_scale);
        }
    }
    if (turn > 0) {
        this.showIlks(this.changes[turn]);
    }
}

Ants.viewNext = function () {
    this.clearIlks(this.changes[this.turn], false);
    this.turn++;
    if (this.turn > this.max_turns) {
        this.turn = 0;
    }
    this.showIlks(this.changes[this.turn]);
}

Ants.viewPrevious = function () {
    this.clearIlks(this.changes[this.turn]);
    this.turn--;
    if (this.turn < 0) {
        this.turn = this.max_turns;
    }
    this.showIlks(this.changes[this.turn]);
}

Ants.clearIlks = function (changes, clear_dead) {
    clear_dead = clear_dead === undefined ? true : clear_dead
    var x_offset = this.canvas.width / 2 - this.x_offset
    var y_offset = this.canvas.height / 2 - this.y_offset
    for (var c = 0, clen = changes.length; c < clen; ++c) {
        var row = changes[c][0];
        var col = changes[c][1];
        var ilk = changes[c][2];
        if (clear_dead && ilk === this.DEAD || ilk !== this.DEAD) {
            this.dc.fillStyle = this.COLOR[this.LAND];
            var x = col * this.x_scale + x_offset;
            var y = row * this.y_scale + y_offset;
            this.dc.fillRect(x, y, this.x_scale, this.y_scale);
        }
    }
}

Ants.showIlks = function (changes) {
    var x_offset = this.canvas.width / 2 - this.x_offset
    var y_offset = this.canvas.height / 2 - this.y_offset
    for (var c = 0, clen = changes.length; c < clen; ++c) {
        var row = changes[c][0];
        var col = changes[c][1];
        var ilk = changes[c][2];
        this.dc.fillStyle = this.COLOR[ilk];
        var x = col * this.x_scale + x_offset;
        var y = row * this.y_scale + y_offset;
        this.dc.fillRect(x, y, this.x_scale, this.y_scale);
    }
}

// initialize visualizer
var init = function () {
    $('#replay_form')[0].reset();
    $.get('0.stream', function (response) {
        Ants.init({data: response, canvas: $('#map')[0]});
        Ants.viewTurn(0);
        animate();
    });
    /*
    $("#replay_file").change(function (evt) {
        var response = evt.target.files[0].getAsText('utf-8');
        Ants.init({data: response, canvas: $('#map')[0]});
        Ants.viewTurn(0);
        animate();
    });
    */

    var running = true;
    var speed = 500;
    var timer = null;
    var animate = function () {
        if (running) {
            forward();
            try {
                timer = setTimeout(animate, speed);
            } catch (ex) {
                alert(ex);
            }
        } else {
            timer = null;
        }
    }
    var show_turn = function () {
        if (Ants.turn === 0) {
            $('#turn').html('start');
        } else if (Ants.turn === Ants.max_turns) {
            $('#turn').html('end');
        } else {
            $('#turn').html('turn ' + Ants.turn);
        }
    }

    var forward = function () {
        Ants.viewNext();
        show_turn();
    };
    
    var backward = function () {
        Ants.viewPrevious();
        $('#turn').html(Ants.turn);
        show_turn();
    };
    
    $(document.documentElement).keydown(function (evt) {
        if (evt.keyCode == '37') { // Left Arrow
            running = false;
            backward();
            return false;
        } else if(evt.keyCode == '39') { // Right Arrow
            running = false;
            forward();
            return false;
        } else if (evt.keyCode == '38') { // Up Arrow
            speed -= 50;
            if (speed <= 0) {
                speed = 50;
            }
        } else if (evt.keyCode == '40') { // Down Array
            speed += 50;
        } else if (evt.keyCode == '32') { // Space
            running = !running;
            if (running && timer == null) {
                animate();
            } else {
                clearTimeout(timer);
                timer = null;
            }
        } else {
            return true;
        }
        return false;
    });
};

//used to stop ie from starting before the canvas is loaded
var ready = true;
var kick_start = function () {
    if (ready) {
        init();
    } else {
        setTimeout(kick_start, 250);
    }
}
$(kick_start);