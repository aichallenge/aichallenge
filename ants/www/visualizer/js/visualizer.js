/**
 * @fileoverview This is a visualizer for the ai challenge ant game.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 * @todo fullscreen mode, onscroll="scroll(0,0)"
 * @todo animated single steps if possible
 * @todo playback controls
 * @todo fog of war option
 * @todo set player colors from replay file (awaiting answer)
 * @todo show when a bot crashed (awaiting answer)
 * @todo borders around the game board
 * @todo some nice graphics and transparency once the layout is good
 * @todo scrolling player names if too long; fade to the left and right
 * @todo zoom in to 20x20 squares with animated ants
 */

/**
 * This global contains constants used throughout the program. I collected them
 * here because NetBeans otherwise clutters the symbol viewer.
 */
Const = {
	/**
	 * This is the relative path to the replay files including tailing '/'
	 */
	REPLAY_PATH: 'visualizer/replays/',
	DRAW_INTERVAL: 50,       // setInterval value
	TIME_PER_TURN: 250,      // in milli seconds
	LEFT_PANEL_W: 100,       // width of left side panel
	TOP_PANEL_H: 30,         // height of top panel
	FOOD_COLOR: [ 200, 200, 150 ],
	PLAYER_COLORS: [
		[ 255,   0,   0 ], [   0,   0, 255 ], [   0, 255,   0 ], [ 255, 255,   0 ],
		[ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ],
		[ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ],
		[ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ],
		[ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ],
		[ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ],
		[ 255, 255, 255 ], [ 255, 255, 255 ]
	]
};

/**
 * The enum for standard graphics is referred to by the imgA and imgB fields of
 * HiFiData and read in the drawing routine of the visualizer.
 */
Img = {
	EMPTY: -2,
	FOOD: -1
};


Quality = {
	LOW: false,
	HIGH: true
};


// translate compass directions to movement offsets
Directions = {
	N : new Pos( 0, -1, 0 / 4 * Math.PI),
	NE: new Pos(+1, -1, 1 / 4 * Math.PI),
	E : new Pos(+1,  0, 2 / 4 * Math.PI),
	SE: new Pos(+1, +1, 3 / 4 * Math.PI),
	S : new Pos( 0, +1, 4 / 4 * Math.PI),
	SW: new Pos(-1, +1, 5 / 4 * Math.PI),
	W : new Pos(-1,  0, 6 / 4 * Math.PI),
	NW: new Pos(-1, -1, 7 / 4 * Math.PI)
};


ParameterType = {
	NONE: 0,
	STRING: 1,
	UINT: 2,
	SCORES: 3,
	LOCATION: 4,
	LOCATION_PLAYER: 5,
	LOCATION_NSEW: 6,
	LOCATION_OPTION: 7
};


function LoFiData(other) {
	this.time = other ? other.time : 0;
	this.x = other ? other.x : 0;
	this.y = other ? other.y : 0;
	this.r = other ? other.r : 0;
	this.g = other ? other.g : 0;
	this.b = other ? other.b : 0;
	this.a = other ? other.a : 0.0;
}
LoFiData.prototype.interpolate = function(other, b) {
	var a = 1.0 - b;
	var result = new LoFiData();
	result.time = a * this.time + b * other.time;
	result.x = a * this.x + b * other.x;
	result.y = a * this.y + b * other.y;
	result.r = (a * this.r + b * other.r) | 0;
	result.g = (a * this.g + b * other.g) | 0;
	result.b = (a * this.b + b * other.b) | 0;
	result.a = a * this.a + b * other.a;
	return result;
};


function HiFiData(other) {
	this.time = other ? other.time : 0;
	this.x = other ? other.x : 0;
	this.y = other ? other.y : 0;
	this.angle = other ? other.angle : Math.random() * 2 * Math.PI;
	this.imgA = other ? other.imgA : Img.EMPTY;
	this.imgB = other ? other.imgB : Img.FOOD;
	this.transition = other ? other.transition : 0;
}
HiFiData.prototype.interpolate = function(other, b) {
	var a = 1.0 - b;
	var result = new HiFiData();
	result.time = a * this.time + b * other.time;
	return result;
}

/**
 * The constructor for on-screen ant objects initializes it with one key frame.
 * @param {number} id the unique object id
 * @param {number} time the time in which the object appears in turn units
 */
function Ant(id, time) {
	this.id = id;
	this.lo = [ new LoFiData() ];
	this.lo[0].time = time;
	this.hi = [ new HiFiData() ];
	this.hi[0].time = time;
	this.loLookup = [];
	this.hiLookup = [];
	this.player = undefined;
}
Ant.prototype.frameAt = function(time, quality, create) {
	var set = quality ? this.hi : this.lo;
	var frame;
	for (var i = set.length - 1; i >= 0; i--) {
		if (set[i].time == time) {
			return set[i];
		} else if (set[i].time < time) {
			if (create) {
				frame = quality ? new HiFiData(set[i]) : new LoFiData(set[i]);
				frame.time = time;
				set.splice(i + 1, 0, frame);
				return frame;
			}
			break;
		}
	}
	return null;
};
/**
 * Interpolates the key frames around the given time and returns the result. If
 * the time exceeds the time stamp of the last key frame, that key frame is
 * returned instead.
 * @param {number} time the time in question
 * @param {Quality} quality selects between the low and high quality set of key
 *     frames
 */
Ant.prototype.interpolate = function(time, quality) {
	var i, delta;
	var set = quality ? this.hi : this.lo;
	var lookup = quality ? this.hiLookup : this.loLookup;
	var timeIdx = time | 0;
	var goFrom = lookup[timeIdx];
	if (goFrom === undefined) {
		for (i = set.length - 1; i >= 0; i--) {
			if (set[i].time <= timeIdx) {
				goFrom = i;
				lookup[timeIdx] = i;
			}
		}
		if (goFrom === undefined) {
			throw 'Can not interpolate before first key frame';
		}
	}
	while (goFrom < set.length - 1 && set[goFrom + 1].time <= time) {
		goFrom++;
	}
	if (goFrom == set.length - 1) {
		return set[goFrom];
	}
	delta = (time - set[goFrom].time) / (set[goFrom + 1].time - set[goFrom].time);
	if (delta == 0) {
		return set[goFrom];
	} else if (delta == 1) {
		return set[goFrom + 1];
	}
	return set[goFrom].interpolate(set[goFrom + 1], delta);
};
// animates the ant ([{<time between 0 and 1>, {<attribute to set absolute>, ...}, {<attribute to set relative>, ...}}, ...])
Ant.prototype.animate = function(list) {
        var key, a, i;
	var interpol = new Array(list.length);
	for (i = 0; i < list.length; i++) {
		var time = this.keyFrames[0].time + list[i].time;
		interpol[i] = this.interpolate(time);
	}
	for (i = 0; i < list.length; i++) {
		for (a = 0; a < this.keyFrames.length; a++) {
			if (this.keyFrames[a].time > time) {
				this.keyFrames.splice(a, 0, interpol[i]);
				break;
			}
		}
		for (key in list[i].absolute) {
			interpol[i][key] = list[i].absolute[key];
		}
		for (key in list[i].relative) {
			interpol[i][key] += list[i].relative[key];
		}
	}
};


// A coordinate set
function Pos(x, y, angle) {
	this.x = x;
	this.y = y;
	this.angle = angle;
}


// A constructor for turns
function Turn() {
	this.spawns = [];
	this.conversions = [];
	this.deaths = [];
	this.existing = [];
	this.orders = [];
}
Turn.prototype.food = function(data, id, prev) {
	// check for food item refresh
	for (var eid in this.existing) {
		if (this.existing[eid].x == data.x && this.existing[eid].y == data.y && this.existing[eid].player === undefined) {
			throw 'A duplicate food item was spawned at (' + data.x + ';' + data.y + ')';
		}
	}
	if (prev !== undefined) {
		prev.spawns.push({id: id, x: data.x, y: data.y});
	}
	this.existing.push({id: id, x: data.x, y: data.y});
	return id + 1;
};
Turn.prototype.ant = function(data, prev) {
	var target = null;
	for (var id in this.existing) {
		if (this.existing[id].x == data.x && this.existing[id].y == data.y) {
			if (this.existing[id].player === undefined) {
				if (target) {
					throw 'Multiple food items at (' + data.x + ';' + data.y + ') found during ant conversion';
				}
				target = this.existing[id];
			} else if (this.existing[id].player == data.player) {
				// command is a refresh for an existing ant
				return;
			} else {
				throw 'An existing ant was found at (' + data.x + ';' + data.y + ') found during ant conversion';
			}
		}
	}
	if (target == null) {
		throw 'No food was found at (' + data.x + ';' + data.y + ') during ant conversion';
	}
	target.player = data.player;
	prev.conversions.push({id: target.id, player: data.player});
};
Turn.prototype.kill = function(data, prev) {
	for (var id in this.existing) {
		if (this.existing[id].x == data.x && this.existing[id].y == data.y && this.existing[id].player == data.player) {
			for (var i = prev.deaths.length - 1; i >= 0; i--) {
				if (prev.deaths[i].x == data.x && prev.deaths[i].y == data.y) {
					prev.deaths[i].ids.push(this.existing[id].id);
					break;
				}
			}
			if (i == -1) {
				prev.deaths.push({x: data.x, y: data.y, ids: [ this.existing[id].id ]});
			}
			delete this.existing[id];
			return;
		}
	}
	throw 'Nothing to kill at (' + data.x + ';' + data.y + ')';
};
Turn.prototype.order = function(data) {
	var moves = 0;
	for (var id in this.existing) {
		if (this.existing[id].x == data.x && this.existing[id].y == data.y) {
			if (this.existing[id].player === undefined) {
				throw 'Attempt to move a food item at (' + data.x + ';' + data.y + ')';
			}
			this.orders.push({id: this.existing[id].id, direction: data.direction});
			moves++;
		}
	}
	if (moves == 0) {
		throw 'No ant to move at (' + data.x + ';' + data.y + ')';
	} else if (moves > 1) {
		throw 'More than one ant moved at (' + data.x + ';' + data.y + ')';
	}		
};
// carry must be called first on a new turn
Turn.prototype.carry = function(prev, width, height) {
	for (var id in prev.existing) {
		this.existing[id] = {
			x: prev.existing[id].x,
			y: prev.existing[id].y,
			id: prev.existing[id].id,
			player: prev.existing[id].player
		};
		for (var i = 0; i < prev.orders.length; i++) {
			var order = prev.orders[i];
			if (this.existing[id].id == order.id) {
				this.existing[id].x = (this.existing[id].x + order.direction.x + width) % width;
				this.existing[id].y = (this.existing[id].y + order.direction.y + height) % height;
				break;
			}
		}
	}
};


function Replay(replayStr) {
	this.replayStr = replayStr;
	this.lines = undefined;
	this.line = undefined;
	this.version = undefined;
	this.players = undefined;
	this.parameters = undefined;
	this.walls = undefined;
	this.turns = undefined;
}
/**
 * Find the next line that isn't empty or a comment line.
 */
Replay.prototype.nonEmptyLine = function(canEnd) {
	var result;
	do {
		this.line++;
		if (this.line >= this.lines.length) {
			if (canEnd) {
				return null;
			} else {
				throw 'The input ended unexpectedly';
			}
		}
		result = this.lines[this.line];
		// 'trim'
		result = result.replace(/^\s\s*/, '').replace(/\s\s*$/, '');
	} while (result == '' || result.charAt(0) == '#');
	return result;
};
/**
 * Adds x and y to a result object by reading tokens 0 and 1.
 */
Replay.prototype.parseLocation = function(tokens, result) {
	result.y = parseInt(tokens[0]);
	if (isNaN(result.y) || result.y < 0 || result.y >= this.parameters.rows) {
		throw 'Y is not an integer within map borders';
	}
	result.x = parseInt(tokens[1]);
	if (isNaN(result.x) || result.x < 0 || result.x >= this.parameters.cols) {
		throw 'X is not an integer within map borders';
	}
};
/**
 * Splits the currently selected line into tokens and checks if the parameters
 * are valid.
 */
Replay.prototype.tokenize = function(line, parameterTypes, allowOthers) {
	// get command type first
	var sp = line.indexOf(' ');
	if (sp == -1) {
		sp = line.length;
	}
	var result = {};
	result.type = line.substr(0, sp).toLowerCase();
	line = line.substr(sp).replace(/^\s\s*/, '').replace(/\s\s*$/, '');
	// parse the rest
	var i;
	var tokens = line.split(' ');
	var parameterType = parameterTypes[result.type];
	if (parameterType === undefined) {
		if (!allowOthers) {
			tokens = [];
			for (parameterType in parameterTypes) {
				tokens.push(parameterType);
			}
			throw 'Unexpected token: ' + result.type + '\n'
				+ 'Valid tokens: ' + tokens;
		}
		parameterType = ParameterType.STRING;
	}
	switch(parameterType) {
		case ParameterType.NONE:
			if (line != '') {
				throw 'No parameter expected';
			}
			break;
		case ParameterType.STRING:
			result.value = line;
			break;
		case ParameterType.UINT:
			result.value = parseInt(line);
			if (isNaN(result.value) || result.value < 0) {
				throw 'Unsigned integer expected';
			}
			break;
		case ParameterType.SCORES:
			result.scores = new Array(tokens.length);
			if (result.scores.length != this.players.length) {
				throw 'Number of scores doesn\' match player count';
			}
			for (i = 0; i < tokens.length; i++) {
				result.scores[i] = parseInt(tokens[i]);
				if (isNaN(result.scores[i])) {
					throw 'Integer scores expected';
				}
			}
			break;
		case ParameterType.LOCATION:
			if (tokens.length != 2) {
				throw 'Location expected';
			}
			this.parseLocation(tokens, result);
			break;
		case ParameterType.LOCATION_PLAYER:
			if (tokens.length != 3) {
				throw 'Location and player id expected';
			}
			this.parseLocation(tokens, result);
			result.player = parseInt(tokens[2]);
			if (isNaN(result.player) || result.player < 0 || result.player >= this.players.count) {
				throw 'Player id is out of range';
			}
			break;
		case ParameterType.LOCATION_OPTION:
			if (tokens.length < 2 || tokens.length > 3) {
				throw 'Location and optional player id expected';
			}
			this.parseLocation(tokens, result);
			if (tokens.length == 3) {
				result.player = parseInt(tokens[2]);
				if (isNaN(result.player) || result.player < 0 || result.player >= this.players.count) {
					throw 'Player id is out of range';
				}
			}
			break;
		case ParameterType.LOCATION_NSEW:
			if (tokens.length != 3) {
				throw 'Location and direction expected';
			}
			this.parseLocation(tokens, result);
			result.direction = Directions[tokens[2].toUpperCase()];
			if (result.direction === undefined) {
				throw 'Direction ' + tokens[2] + ' is undefined';
			}
			break;
	}
	return result;
};
Replay.prototype.parse = function() {
	var row, col, i, k, c, f1, f2, collision, spawn, order, conversion;
	var deathList, playerId, antObj;
	var id = 0;      // 'auto-increment' value for ant ids
	var tturns = []; // for each turn, contains a basic data structure containing all the information from <replayStr>
	var t = 0;       // keeps track of the current turn, during parsing
	var trimmed;     // an input line without comments and leading or trailing whitespace
	var result;      // a tokenized input line
	var validTokens; // tokens understood in the current context (depending on player count)
	var endTurn = false;

	// convert replayStr into tokens per line
	this.lines = this.replayStr.split('\n');
	this.line = -1;
	try {
		// check version
		trimmed = this.nonEmptyLine();
		this.version = this.tokenize(trimmed, {version: ParameterType.UINT}).value;
		if (this.version < 1 || this.version > 1) {
			throw 'File version ' + this.version + ' cannot be read by this visualizer';
		}
		// turns
		trimmed = this.nonEmptyLine();
		// initialization turn '0'
		if (this.tokenize(trimmed, {turn: ParameterType.UINT}).value != t) {
			throw 'Expected turn ' + t;
		}
		// read parameters and map data
		validTokens = {
			players     : ParameterType.UINT,
			rows        : ParameterType.UINT,
			cols        : ParameterType.UINT,
			m           : ParameterType.STRING,
			viewradius  : ParameterType.UINT,
			attackradius: ParameterType.UINT,
			birthradius : ParameterType.UINT,
			antvalue    : ParameterType.UINT,
			gameid      : ParameterType.UINT,
			gamelocation: ParameterType.STRING,
			turn        : ParameterType.UINT,
			end         : ParameterType.NONE
		};
		this.parameters = {
			// these undefined parameters must be defined by the replay
			cols: undefined,
			rows: undefined,
			// these values need not be defined and have defaults
			viewradius: 16,
			attackradius: 4,
			birthradius: 1,
			antvalue: 1,
			gameid: null,
			gamelocation: 'local'
		};
		this.players = undefined;
		this.walls = [];
		tturns.push(new Turn());
		while (true) {
			trimmed = this.nonEmptyLine();
			result = this.tokenize(trimmed, validTokens, true);
			if (result.type == 'turn' || result.type == 'end') {
				break;
			} else if (result.type == 'm') {
				if (this.parameters.cols === undefined || this.parameters.rows === undefined) {
					throw 'Cannot parse map data until rows and cols are defined';
				}
				if (result.value.length != this.parameters.cols) {
					throw 'Map row length (' + result.value.length + ') doesn\'t match cols parameter (' + this.parameters.cols + ')';
				}
				row = new Array(this.parameters.cols);
				for (col = 0; col < this.parameters.cols; col++) {
					c = result.value.charAt(col);
					row[col] = (c == '%');
					if (c >= 'a' && c <= 'z') {
						id = tturns[0].food({x: col, y: this.walls.length}, id);
					}
				}
				this.walls.push(row);
			} else if (result.type.search('^player[1-9][0-9]*') == 0) {
				playerId = result.type.match('[1-9][0-9]*');
				result.type = result.type.substr('player'.length + playerId.length);
				playerId = parseInt(playerId) - 1;
				this.players[playerId][result.type] = result.value;
			} else {
				this.parameters[result.type] = result.value;
				if (result.type == 'players') {
					this.players = new Array(this.parameters.players);
					for (i = 0; i < this.parameters.players; i++) {
						this.players[i] = {name: 'Player ' + (i + 1)};
						validTokens['player' + (i + 1) + 'name'] = ParameterType.STRING;
						validTokens['player' + (i + 1) + 'submitid'] = ParameterType.UINT;
					}
				}
			}
		}
		if (this.parameters.cols === undefined || this.parameters.rows === undefined || this.players === undefined) {
			throw 'Cols, rows and players are requires parameters in turn 0';
		}
		if (this.walls.length != this.parameters.rows) {
			throw 'Number of map rows doesn\'t match rows parameter';
		}
		this.turns = [];
		validTokens = {
			a   : ParameterType.LOCATION_PLAYER, // ant spawn
			f   : ParameterType.LOCATION,        // food spawn
			d   : ParameterType.LOCATION_OPTION, // ant or food died last round
			o   : ParameterType.LOCATION_NSEW,   // move order
			turn: ParameterType.UINT,
			end : ParameterType.NONE
		};
		// other turns
		this.turns.push({ants: [], counts: new Array(this.players.length)});
		while (!endTurn) {
			t++;
			if (result.type == 'turn' && result.value != t) {
				throw 'Expected turn ' + t;
			} else if (result.type == 'end') {
				endTurn = true;
			}
			tturns.push(new Turn());
			tturns[t].carry(tturns[t - 1], this.parameters.cols, this.parameters.rows);
			this.turns.push({ants: [], counts: new Array(this.players.length)});
			trimmed = this.nonEmptyLine();
			result = this.tokenize(trimmed, {score: ParameterType.SCORES});
			this.turns[t].scores = result.scores.slice(0);
			while (result.type != 'turn' && result.type != 'end') {
				trimmed = this.nonEmptyLine(endTurn);
				if (trimmed == null) { // this is the end of file check
					break;
				}
				result = this.tokenize(trimmed, validTokens);
				switch (result.type) {
					case 'a':
						tturns[t].ant(result, tturns[t - 1]);
						break;
					case 'f':
						id = tturns[t].food(result, id, tturns[t - 1]);
						break;
					case 'd':
						tturns[t].kill(result, tturns[t - 1]);
						break;
					case 'o':
						tturns[t].order(result);
						break;
				}
			}
		}
	} catch (error) {
		alert(error + '\nIn line ' + (this.line + 1) + ': ' + this.lines[this.line]);
	}

	// Add ants...
	var nextAntDirection = function(id, turn) {
		for (var nadk = 0; nadk < tturns[turn + 1].orders.length; nadk++) {
			var nadAction = tturns[turn + 1].orders[nadk];
			if (nadAction.id == id) {
				return nadAction.direction;
			}
		}
		return null;
	}
	// we need the existing dummy food items from turn '0'
	var antStates = {};
	for (id in tturns[0].existing) {
		spawn = tturns[0].existing[id];
		antObj = new Ant(spawn.id, 0);
		with (antObj.frameAt(0, Quality.LOW)) {
			x = spawn.x;
			y = spawn.y;
			r = Const.FOOD_COLOR[0];
			g = Const.FOOD_COLOR[1];
			b = Const.FOOD_COLOR[2];
			a = 1.0;
		}
		antStates[antObj.id] = antObj;
	}
	for (t = 0; t < this.turns.length - 1; t++) {
		// movement
		for (i = 0; i < tturns[t].orders.length; i++) {
			order = tturns[t].orders[i];
			antObj = antStates[order.id];
			antObj.frameAt(t - 1, Quality.LOW, true);
			f2 = antObj.frameAt(t - 0.5, Quality.LOW, true);
			f2.x += order.direction.x;
			f2.y += order.direction.y;
			/*antObj.keyFrames[1].angle = offset.angle;
			antObj.animate([{
				time: 0.5,
				absolute: {
					x: antObj.keyFrames[1].x,
					y: antObj.keyFrames[1].y//,
					//angle: offset.angle
				},
				relative: {}
			}]);
			// only in zoom mode: < movement time reduced to 0.5, pay attention when uncommenting >
			/*
			nextDir = nextAntDirection(antObj.id, t);
			if (nextDir) {
				var angle = nextDir.angle - offset.angle;
				if (angle < -Math.PI) {
					angle += 2 * Math.PI;
				} else if (angle > +Math.PI) {
					angle -= 2 * Math.PI;
				}
				if (angle != 0) {
					antObj.keyFrames[1].jitter = 0;
					antObj.keyFrames[2].jitter = 0;
					if (Math.abs(angle) < 0.9 * Math.PI) {
						var sq = 1 / Math.sqrt(2);
						antObj.keyFrames[2].x = antObj.keyFrames[1].x + 0.5 * (offset.x * sq + (1 - sq) * nextDir.x);
						antObj.keyFrames[2].y = antObj.keyFrames[1].y + 0.5 * (offset.y * sq + (1 - sq) * nextDir.y);
						antObj.keyFrames[2].angle += 0.5 * angle;
					}
				}
			}*/
		}
		// spawn
		for (i = 0; i < tturns[t].spawns.length; i++) {
			spawn = tturns[t].spawns[i];
			antObj = new Ant(spawn.id, t - 1)
			f1 = antObj.frameAt(t - 1, Quality.LOW);
			f1.x = spawn.x;
			f1.y = spawn.y;
			f1.r = Const.FOOD_COLOR[0];
			f1.g = Const.FOOD_COLOR[1];
			f1.b = Const.FOOD_COLOR[2];
			antObj.frameAt(t - 0.25, Quality.LOW, true);
			f2 = antObj.frameAt(t, Quality.LOW, true);
			f2.a = 1.0;
//					nextDir = nextAntDirection(antObj.id, t);
//					if (nextDir) {
//						antObj.keyFrames[1].angle = nextDir.angle;
//					}
			this.turns[t].ants.push(antObj);
			antStates[antObj.id] = antObj;
		}
		// conversion
		for (i = 0; i < tturns[t].conversions.length; i++) {
			conversion = tturns[t].conversions[i];
			antObj = antStates[conversion.id];
			antObj.player = conversion.player;
			antObj.frameAt(t - 0.25, Quality.LOW, true);
			f2 = antObj.frameAt(t, Quality.LOW, true);
//					nextDir = nextAntDirection(antObj.id, t);
//					if (nextDir) {
//						antObj.keyFrames[1].angle = nextDir.angle;
//					}
//					antObj.animate([{time: 0.75, absolute: {}, relative: {}}]);
			f2.r = Const.PLAYER_COLORS[conversion.player][0];
			f2.g = Const.PLAYER_COLORS[conversion.player][1];
			f2.b = Const.PLAYER_COLORS[conversion.player][2];
		}
		// kills
		for (i = 0; i < tturns[t].deaths.length; i++) {
			deathList = tturns[t].deaths[i].ids;
			collision = deathList.length > 1;
			for (k = 0; k < deathList.length; k++) {
				antObj = antStates[deathList[k]];
				if (collision) {
					antObj.frameAt(t - 0.75, Quality.LOW, true);
					f2 = antObj.frameAt(t - 0.5, Quality.LOW, true);
				} else {
					antObj.frameAt(t - 0.25, Quality.LOW, true);
					f2 = antObj.frameAt(t, Quality.LOW, true);
				}
				f2.a = 0.0;
				delete antStates[deathList[k]];
			}
		}
		// Copy the last ant state to the next level
		for (i = 0; i < this.players.length; i++) {
			this.turns[t + 1].counts[i] = 0;
		}
		for (i in antStates) {
			this.turns[t + 1].ants.push(antStates[i]);
			if (antStates[i].player !== undefined) {
				this.turns[t + 1].counts[antStates[i].player]++;
			}
		}
	}
};

/**
 * the main 'application' object
 */
visualizer = {

	canvas: undefined,          // the main canvas
	ctx2d: undefined,           // ...and it's 2D context
	canvasBackground: undefined,// contains the backdrop for the map
	ctx2dBackground: undefined, // ...associated 2D context

	turns: undefined,           // array of precomputed turn data
	intervalDraw: undefined,    // handle to the redraw timer
	timeOffset: undefined,      // playback time is relative to this
	timeOld: undefined,         // used to find turn transitions
	turn: undefined,
	playdir: undefined,
	w: undefined,
	h: undefined,
	scale: undefined,
	parameters: {},             // GET parameters from the URL

	images : {
		ants: [ new Image(), new Image() ],  // the two ant images
		sands: [ new Image(), new Image() ], // the two background textures
		distributionMap: new Image(),        // interpolation map for the textures
		wall: new Image(),                   // image of the wall
		base: new Image(),                   // image of the player base

		request: function() {
/*			var sandA = Random.range(SANDS);
			var sandB = (sandA + 1 + Random.range(SANDS - 1)) % SANDS;
			this.sands[0].src = 'sand_' + sandA + '.jpg';
			this.sands[1].src = 'sand_' + sandB + '.jpg';
			this.distributionMap.src = 'distribution.jpg';
			this.wall.src = 'wall.png';
			this.base.src = 'base.png';
			this.ants[0].src = 'ant_0.png';
			this.ants[1].src = 'ant_1.png';*/
		},

		complete: function() {
			return true;
/*			return this.wall.complete && this.base.complete
				&& this.distributionMap.complete
				&& this.sands[0].complete && this.sands[1].complete
				&& this.ants[0].complete && this.ants[1].complete;*/
		}
	},

	// reads URL parameters and stores them in the parameters object
	parseUrl: function() {
		var equalPos, value, parameter, i;
		var parameters = window.location.href.split('?');
		if (parameters.length > 1) {
			parameters = parameters[1].split('#')[0].split('&');
			for (i = 0; i < parameters.length; i++) {
				equalPos = parameters[i].indexOf('=');
				parameter = parameters[i].substr(0, equalPos);
				value = parameters[i].substr(equalPos + 1);
				this.parameters[parameter] = value;
			}
		}
	},

	// loads the replay file named by the game_id parameter in the URL
	loadReplayDataFromGameId: function() {
		this.loadReplayDataFromFile(Const.REPLAY_PATH + this.parameters.game_id + '.replay');
	},

	loadReplayDataFromURI: function(file) {
		window.clearInterval(this.intervalDraw);
		this.intervalDraw = undefined;
		this.timeOffset = undefined;
		this.turn = undefined;
		var p = new XMLHttpRequest();
		p.open("GET", file, false);
		p.send(null);
		this.replay = new Replay(p.responseText);
	},

	loadReplay: function(data, unescape) {
		window.clearInterval(this.intervalDraw);
		this.intervalDraw = undefined;
		this.timeOffset = undefined;
		this.turn = undefined;
		if (unescape) {
			data = data.replace(/&lt;/g, '<').replace(/&gt;/g, '>')
			data = data.replace(/&#0*39;/g, "'").replace(/&quot;/g, '"');
			data = data.replace(/&amp;/g, '&').replace(/\\n/g, '\n');
		}
		this.replay = new Replay(data);
	},

	attach: function(container, width, height) {
		this.w = width;
		this.h = height;
		this.replay.parse();
		if (this.canvas === undefined) {
			this.canvas = document.createElement('canvas');
			document.getElementById(container).appendChild(this.canvas);
		}
		this.ctx2d = this.canvas.getContext('2d');
		document.onkeydown = function(event) {
			if (event.keyCode == 37) {
				visualizer.stepBw();
			} else if (event.keyCode == 39) {
				visualizer.stepFw();
			} else if (event.keyCode == 32) {
				visualizer.play();
			} else if (event.keyCode == 33) {
				visualizer.stepBw(10);
			} else if (event.keyCode == 34) {
				visualizer.stepFw(10);
			} else if (event.keyCode == 36) {
				visualizer.first();
			} else if (event.keyCode == 35) {
				visualizer.last();
			}
		};
		window.onresize = function(event) {
			//alert('resized the window');
		}
		this.initWaitForImages();
	},

	initRequestImages: function() {
		this.images.request();
	},

	initWaitForImages: function() {
		if (!this.images.complete()) {
			window.setTimeout('visualizer.initWaitForImages()', 50);
			return;
		}
		if (this.canvasBackground === undefined) {
			this.canvasBackground = document.createElement('canvas');
		}
		var iw = (this.w ? this.w : window.innerWidth) - Const.LEFT_PANEL_W;
		var ih = (this.h ? this.h : window.innerHeight) - Const.TOP_PANEL_H;
		this.scale = Math.min(10, Math.max(1, Math.min(iw / this.replay.parameters.cols, ih / this.replay.parameters.rows))) | 0;
		var pw = this.scale * this.replay.parameters.cols + Const.LEFT_PANEL_W;
		var ph = this.scale * this.replay.parameters.rows + Const.TOP_PANEL_H;
		this.canvasBackground.width = pw;
		this.canvasBackground.height = ph;
		this.ctx2dBackground = this.canvasBackground.getContext('2d');
		this.ctx2dBackground.fillStyle = '#EEE';
		this.ctx2dBackground.fillRect(0, 0, pw, ph);
		this.ctx2dBackground.fillStyle = '#000';
		for (var row = -1; row <= this.replay.parameters.rows; row++) {
			for (var col = -1; col <= this.replay.parameters.cols; col++) {
				if (this.replay.walls[(row + this.replay.parameters.rows) % this.replay.parameters.rows][(col + this.replay.parameters.cols) % this.replay.parameters.cols]) {
					this.ctx2dBackground.fillRect(this.scale * col, this.scale * row, this.scale, this.scale);
				}
			}
		}
		this.canvas.width = this.canvasBackground.width;
		this.canvas.height = this.canvasBackground.height;
		this.ctx2d.textAlign = 'center';
		this.ctx2d.textBaseline = 'middle';
		this.ctx2d.font = '10px sans-serif';
		this.playdir = 0;
		this.intervalDraw = window.setInterval('visualizer.draw()', Const.DRAW_INTERVAL);
	},

	drawColorBar: function(x, y, w, h, values, colors) {
		var sum = 0;
		for (var i = 0; i < values.length; i++) {
			sum += values[i];
		}
		var useValues = values;
		if (sum == 0) {
			useValues = new Array(values.length);
			for (i = 0; i < values.length; i++) {
				useValues[i] = 1;
			}
			sum = values.length;
		} 
		var scale = h / sum;
		var offset = y;
		for (i = 0; i < useValues.length; i++) {
			var amount = scale * useValues[i];
			this.ctx2d.fillStyle = 'rgb(' + colors[i][0] + ', ' + colors[i][1] + ', ' + colors[i][2] + ')';
			this.ctx2d.fillRect(x, offset, w, amount);
			offset += amount;
		}
		this.ctx2d.textAlign = 'right';
		this.ctx2d.textBaseline = 'top';
		this.ctx2d.font = 'bold 20px sans-serif';
		this.ctx2d.fillStyle = 'rgba(0,0,0,0.5)';
		offset = y;
		for (i = 0; i < useValues.length; i++) {
			this.ctx2d.fillText(values[i], x + w, offset)
			offset += scale * useValues[i];
		}
	},

	draw: function() {
		//~ var timeA = new Date().getTime();
		//ctx2d.globalCompositeOperation = 'copy';
		//ctx2d.globalCompositeOperation = 'source-over';
		var time = new Date().getTime();
		var drawOrder = new Array();
		var transition, mx, my, anim;
		if (this.timeOffset === undefined) {
			this.timeOffset = time;
			transition = true;
		}
		this.ctx2d.drawImage(this.canvasBackground, Const.LEFT_PANEL_W, Const.TOP_PANEL_H);
		var turn;
		if (this.intervalDraw === undefined) {
			turn = this.turn;
			time = turn - 1;
			transition = true;
		} else {
			time = (time - this.timeOffset) / Const.TIME_PER_TURN;
			if (time < 0 || time >= this.replay.turns.length - 2) {
				/* The timer ran beyond the last turn or the clock changed to
				 * an invalid time in the past. In any case we want to display
				 * the end result of the game.
				 */
				turn = this.replay.turns.length - 1;
				time = turn - 1;
				anim = 0;
				transition = true;
				window.clearInterval(this.intervalDraw);
				this.intervalDraw = undefined;
			} else {
				turn = (time | 0) + 1;
				anim = time - (time | 0);
				transition = transition || ((time | 0) != (this.timeOld | 0));
			}
		}
		/* Repaint the map rows if we have a transition from on turn to
		 * another.
		 */
		if (transition) {
			//document.getElementById('lblTurn').innerHTML = ((turn > this.replay.turns.length - 2) ? 'end result' : turn + ' / ' + (this.replay.turns.length - 2));
			var w = Const.LEFT_PANEL_W / 2;
			var h = this.canvasBackground.height - 20 - Const.TOP_PANEL_H;
			this.ctx2d.fillStyle = '#000';
			this.ctx2d.fillRect(0, Const.TOP_PANEL_H, 2 * w, this.canvasBackground.height);
			this.ctx2d.fillRect(0, 0, this.canvasBackground.width, Const.TOP_PANEL_H);
			var colors = Const.PLAYER_COLORS;
			this.ctx2d.textAlign = 'left';
			this.ctx2d.textBaseline = 'top';
			this.ctx2d.font = 'bold 20px sans-serif';
			var x = 0;
			for (i = 0; i < this.replay.players.length; i++) {
				this.ctx2d.fillStyle = 'rgb(' + colors[i][0] + ', ' + colors[i][1] + ', ' + colors[i][2] + ')';
				this.ctx2d.fillText(this.replay.players[i].name, x, 0);
				x += this.ctx2d.measureText(this.replay.players[i].name).width;
				if (i != this.replay.players.length - 1) {
					this.ctx2d.fillStyle = '#888';
					this.ctx2d.fillText(' vs ', x, 0);
					x += this.ctx2d.measureText(' vs ').width;
				}
			}
			this.ctx2d.fillStyle = '#FFF';
			this.ctx2d.textAlign = 'center';
			this.ctx2d.textBaseline = 'middle';
			this.ctx2d.font = 'bold 12px sans-serif';
			this.ctx2d.fillText('ants', w / 2, Const.TOP_PANEL_H + 10);
			this.ctx2d.fillText('scores', 3 * w / 2, Const.TOP_PANEL_H + 10);
			this.drawColorBar(2    , Const.TOP_PANEL_H + 20, w - 4, h - 2, this.replay.turns[turn].counts, Const.PLAYER_COLORS);
			this.drawColorBar(2 + w, Const.TOP_PANEL_H + 20, w - 4, h - 2, this.replay.turns[turn].scores, Const.PLAYER_COLORS);
		}
		for (var i = 0; i < this.replay.turns[turn].ants.length; i++) {
			var antObj = this.replay.turns[turn].ants[i].interpolate(time, Quality.LOW);
			drawOrder.push(antObj);
		}
		this.ctx2dBackground.globalAlpha = 100 / Const.TIME_PER_TURN;
		if (Math.random() < 0.5) {
			this.ctx2dBackground.strokeStyle = '#222';
		} else {
			this.ctx2dBackground.strokeStyle = '#CA7';
		}
		this.ctx2dBackground.beginPath();
		this.ctx2d.textBaseline = 'top';
		for (var n = 0; n < drawOrder.length; n++) {
			antObj = drawOrder[n];
			this.ctx2d.save();
			if (true) {
				mx = Math.round(this.scale * antObj.x + Const.LEFT_PANEL_W);
				my = Math.round(this.scale * antObj.y + Const.TOP_PANEL_H);
				this.ctx2d.fillStyle = 'rgba(' + antObj.r + ', ' + antObj.g + ', ' + antObj.b + ', ' + antObj.a + ')';
				this.ctx2d.fillRect(mx, my, this.scale, this.scale);
			} else {
				this.ctx2d.globalAlpha = antObj.alpha;
				this.ctx2d.translate(10 + 20 * antObj.x, 10 + 20 * antObj.y);
				this.ctx2d.rotate(antObj.angle + Math.sin(20 * time) * antObj.jitter);
				this.ctx2d.drawImage(this.images.ants[antObj.type], -10, -10);
				this.ctx2d.restore();
				mx = 20 * antObj.x + 10 + 3 * Math.tan(2 * (Math.random() - 0.5));
				my = 20 * antObj.y + 10 + 3 * Math.tan(2 * (Math.random() - 0.5));
				if (antObj.alpha == 1) {
					var sin = -Math.sin(antObj.angle);
					var cos = +Math.cos(antObj.angle);
					this.ctx2dBackground.moveTo(mx - sin, my - cos);
					this.ctx2dBackground.lineTo(mx + sin, my + cos);
				}
			}
		}
		this.ctx2dBackground.stroke();
		this.timeOld = time;
		//~ var timeB = new Date().getTime();
		//~ document.title = 'Render time: ' + (timeB - timeA) + ' ms';
		//~ return;
	},
	
	stepHelper: function(isForward) {
		window.clearInterval(this.intervalDraw);
		this.intervalDraw = undefined;
		if (this.turn === undefined) {
			var time = (new Date().getTime() - this.timeOffset) / Const.TIME_PER_TURN;
			this.turn = (time | 0) + (isForward ? 1 : 2);
		}
	},

	stepFw: function(steps) {
		this.stepHelper(true);
		this.turn += (steps === undefined) ? 1 : steps;
		this.playdir = 0;
		if (this.turn > this.replay.turns.length - 1) {
			this.turn = this.replay.turns.length - 1;
		}
		this.draw();
	},

	stepBw: function(steps) {
		this.stepHelper(false);
		this.turn -= (steps === undefined) ? 1 : steps;
		this.playdir = 1;
		if (this.turn < 1) {
			this.turn = 1;
		} else if (this.turn > this.replay.turns.length - 2) {
			this.turn = this.replay.turns.length - 2;
		}
		this.draw();
	},
	
	first: function() {
		this.stepHelper(false);
		this.turn = 1;
		this.draw();
	},
	
	last: function() {
		this.stepHelper(true);
		this.turn = this.replay.turns.length - 1;
		this.draw();
	},

	play: function() {
		if (this.intervalDraw === undefined) {
			if (this.turn === undefined || this.turn == this.replay.turns.length - 1) {
				this.timeOffset = new Date().getTime();
			} else {
				this.timeOffset = new Date().getTime() - (this.turn - 1) * Const.TIME_PER_TURN;
			}
			this.turn = undefined;
			this.intervalDraw = window.setInterval('visualizer.draw()', Const.DRAW_INTERVAL);
			this.playdir = 0;
		} else {
			window.clearInterval(this.intervalDraw);
			this.intervalDraw = undefined;
			var time = (new Date().getTime() - this.timeOffset) / Const.TIME_PER_TURN;
			this.turn = (time | 0) + 1 + this.playdir;
			this.draw();
		}
	}

};


var Random = {

	// returns an integer in the range [0..range[
	range: function(range) {
		return Math.random() * range | 0;
	},

	// returns an integer in the range [from..to]
	fromTo: function(from, to) {
		return from + (Math.random() * (1 + to - from) | 0);
	}

};