/**
 * @fileoverview This is a visualizer for the ai challenge ant game.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 * @todo animated single steps if possible
 * @todo playback controls
 * @todo fog of war option
 * @todo borders around the game board
 * @todo some nice graphics and transparency once the layout is good
 * @todo scrolling player names if too long; fade to the left and right
 * @todo zoom in to 20x20 squares with animated ants
 * @todo save settings
 * @todo set player colors from replay file (awaiting answer)
 * @todo show when a bot crashed (awaiting answer)
 * @todo clickable player names (requires user_id)
 */

/**
 * This global contains constants used throughout the program. I collected them
 * here because NetBeans otherwise clutters the symbol viewer.
 */
Const = {
	DRAW_INTERVAL: 50,       // setInterval value
	TIME_PER_TURN: 250,      // in milli seconds
	LEFT_PANEL_W: 100,       // width of left side panel
	TOP_PANEL_H: 30,         // height of top panel
	FOOD_COLOR: [ 200, 200, 150 ],
	PLAYER_COLORS: [
		[ 255,   0,   0 ], [   0,   0, 255 ], [   0, 180,   0 ], [ 255, 200,   0 ],
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
	EMPTY: 26,
	FOOD: 27
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


PlayDir = {
	FORWARD: 0,
	BACKWARD: 1
};


Key = {
	LEFT: 37,
	RIGHT: 39,
	SPACE: 32,
	PGUP: 33,
	PGDOWN: 34,
	HOME: 36,
	END: 35
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
	this.player = undefined;
	/** @private */
	this.loLookup = [];
	/** @private */
	this.hiLookup = [];
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


function LineIterator(textBlock) {
	/** @private */
	this.lines = textBlock.split('\n');
	/** @private */
	this.line = -1;
}
/**
 * Find the next line that isn't empty or a comment line.
 * @param {boolean} canEnd if true, returns an empty string on the end of file
 *     instead of throwing an error.
 * @type string
 */
LineIterator.prototype.nonEmptyLine = function(canEnd) {
	var result;
	do {
		this.line++;
		if (this.line >= this.lines.length) {
			if (canEnd) {
				return '';
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
 * returns the line selected by the last call to nonEmptyLine
 * @type string
 */
LineIterator.prototype.current = function() {
	return this.lines[this.line];
};
/**
 * returns the current line number
 * @type number
 */
LineIterator.prototype.currentNum = function() {
	return this.line + 1;
};


/**
 * keeps track of persistent configuration values
 */
function Config() {
	this.fullscreen = false;
	this.load();
}
/**
 * checks if local storage is available
 * @private
 */
Config.prototype.hasLocalStorage = function() {
	try {
		return 'localStorage' in window && window['localStorage'] !== null;
	} catch (error) {
		return error;
	}
};
/**
 * stores all keys to local storage if supported
 * @return true, if successful, an error if not
 */
Config.prototype.save = function() {
	if (this.hasLocalStorage() === true) {
		for (key in this) {
			if (this[key] === undefined) {
				delete window.localStorage['visualizer.' + key];
			} else if (this[key] === null) {
				window.localStorage['visualizer.' + key] = "null";
			} else {
				var prefix = undefined;
				switch (typeof this[key]) {
					case 'number':
						prefix = 'N';
						break;
					case 'boolean':
						prefix = 'B';
						break;
					case 'string':
						prefix = 'S';
						break;
					case 'function':
						break;
					default:
						alert(key + ': ' + (typeof this[key]))
						throw 'Only numbers, booleans and strings can be saved in the config';
				}
				if (prefix) {
					window.localStorage['visualizer.' + key] = prefix + this[key];
				}
			}
		}
		return true;
	} else {
		return false;
	}
};
/**
 * Tries to load config values from local storage if available. Registred
 * functions on value change will fire.
 */
Config.prototype.load = function() {
	if (this.hasLocalStorage() === true) {
		for (key in this) {
			var val = window.localStorage['visualizer.' + key];
			if (typeof this[key] != 'function' && val) {
				if (val === 'null') {
					this[key] = null;
				} else {
					switch (val.charAt(0)) {
						case 'N':
							this[key] = Number(val.substr(1));
							break;
						case 'B':
							this[key] = (val == 'Btrue');
							break;
						case 'S':
							this[key] = val.substr(1);
							break;
					}
				}
			}
		}
		return true;
	} else {
		return false;
	}
};
Config.prototype.clear = function() {
	if (this.hasLocalStorage() === true) {
		window.localStorage.clear();
	}
}


function Replay(replayStr) {
	this.version = undefined;
	this.players = undefined;
	this.parameters = {};
	this.walls = undefined;
	this.turns = undefined;

	var row, col, i, k, c, f1, f2, collision, spawn, order, conversion;
	var deathList, playerId, antObj;
	var id = 0;      // 'auto-increment' value for ant ids
	var tturns = []; // for each turn, contains a basic data structure containing all the information from <replayStr>
	var t = 0;       // keeps track of the current turn, during parsing
	var trimmed;     // an input line without comments and leading or trailing whitespace
	var result;      // a tokenized input line
	var validTokens; // tokens understood in the current context (depending on player count)
	var endTurn = false;
	var	lines = new LineIterator(replayStr);
	try {
		// check version
		trimmed = lines.nonEmptyLine();
		this.version = this.tokenize(trimmed, {version: ParameterType.UINT}).value;
		if (this.version < 1 || this.version > 1) {
			throw 'File version ' + this.version + ' cannot be read by this visualizer';
		}
		// turns
		trimmed = lines.nonEmptyLine();
		// initialization turn '0'
		if (this.tokenize(trimmed, {turn: ParameterType.UINT}).value != t) {
			throw 'Expected turn ' + t;
		}
		// read parameters and map data
		validTokens = {};
		validTokens.players       = ParameterType.UINT;
		validTokens.rows          = ParameterType.UINT;
		validTokens.cols          = ParameterType.UINT;
		validTokens.m             = ParameterType.STRING;
		validTokens.viewradius    = ParameterType.UINT;
		validTokens.vattackradius = ParameterType.UINT;
		validTokens.birthradius   = ParameterType.UINT;
		validTokens.antvalue      = ParameterType.UINT;
		validTokens.gameid        = ParameterType.UINT;
		validTokens.gamelocation  = ParameterType.STRING;
		validTokens.turn          = ParameterType.UINT;
		validTokens.end           = ParameterType.NONE;
		// these undefined parameters must be defined by the replay
		this.parameters.cols = undefined;
		this.parameters.rows = undefined;
		// these values need not be defined and have defaults
		this.parameters.viewradius = 16;
		this.parameters.attackradius = 4;
		this.parameters.birthradius = 1;
		this.parameters.antvalue = 1;
		this.parameters.gameid = null;
		this.parameters.gamelocation = 'local';
		this.players = undefined;
		this.walls = [];
		tturns.push(new Turn());
		while (true) {
			trimmed = lines.nonEmptyLine();
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
		validTokens = {};
		validTokens.a    = ParameterType.LOCATION_PLAYER; // ant spawn
		validTokens.f    = ParameterType.LOCATION;        // food spawn
		validTokens.d    = ParameterType.LOCATION_OPTION; // ant or food died last round
		validTokens.o    = ParameterType.LOCATION_NSEW;   // move order
		validTokens.turn = ParameterType.UINT;
		validTokens.end  = ParameterType.NONE;
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
			trimmed = lines.nonEmptyLine();
			result = this.tokenize(trimmed, {score: ParameterType.SCORES});
			this.turns[t].scores = result.scores.slice(0);
			while (result.type != 'turn' && result.type != 'end') {
				trimmed = lines.nonEmptyLine(endTurn);
				if (!trimmed) { // this is the end of file check
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
		//throw 'In line #' + lines.currentNum() + ' "' + lines.current() + '": ' + error;
	}

	// Add animation...
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
}
/**
 * Adds x and y to a result object by reading tokens 0 and 1.
 * @private
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
 * @private
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


function ImageManager() {
	this.images = [];
}
ImageManager.prototype.request = function(source) {
	var image = new Image();
	image.src = source;
	this.images.push(image);
	return this.images.length - 1;
};
ImageManager.prototype.complete = function(id) {
	if (id) {
		return this.images[id].complete;
	} else {
		var result = true;
		for (id = 0; id < this.images.length && result; id++) {
			result &= this.images[id].complete;
		}
		return result;
	}
};


/**
 * The main 'application' object that provides all necessary methods for the use
 * in a web page.
 * @param {Node} container the html element, that the visualizer will embed into
 */
function Visualizer(container) {
	/**
	 * any generated DOM elements will be placed here
	 * @private
	 */
	this.container = container;
	/**
	 * the main canvas
	 * @private
	 */
	this.canvas = document.createElement('canvas');
	/**
	 * ...and it's 2D context
	 * @private
	 */
	this.ctx2d = this.canvas.getContext('2d');
	/**
	 * contains the backdrop for the map
	 * @private
	 */
	this.canvasBackground = undefined;
	/**
	 * ...associated 2D context
	 * @private
	 */
	this.ctx2dBackground = undefined;
	/**
	 * array of precomputed turn data
	 * @private
	 */
	this.turns = undefined;
	/**
	 * handle to the redraw timer
	 * @private
	 */
	this.intervalDraw = undefined;
	/**
	 * playback time is relative to this
	 * @private
	 */
	this.timeOffset = undefined;
	/**
	 * used to find turn transitions
	 * @private
	 */
	this.timeOld = undefined;
	/**
	 * set to a turn number if single stepping is in effect
	 * @private
	 */
	this.turn = undefined;
	/**
	 * play direction (forward or backward)
	 * @private
	 */
	this.playdir = undefined;
	/**
	 * usable width for the visualizer
	 * @private
	 */
	this.w = undefined;
	/**
	 * usable height for the visualizer
	 * @private
	 */
	this.h = undefined;
	/**
	 * size of an ant in pixels
	 * @private
	 */
	this.scale = undefined;
	/**
	 * images used by the visualizer
	 * @private
	 */
	this.images = new ImageManager();
	/**
	 * presistable configuration values
	 * @private
	 */
	this.config = new Config();
	/**
	 * GET parameters from the URL
	 * @private
	 */
	this.parameters = {};
	// read URL parameters and store them in the parameters object
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
}
/**
 * Loads a replay file located on the same server using a XMLHttpRequest.
 * @param {string} file the relative file name
 */
Visualizer.prototype.loadReplayDataFromURI = function(file) {
	var p = new XMLHttpRequest();
	p.open("GET", file, false);
	p.send(null);
	this.loadReplayData(p.responseText);
};
/**
 * Loads a replay file through the php site. This data has html special
 * characters encoded and has a general structure of key=value.
 * @param {string} data the encoded replay data
 */
Visualizer.prototype.loadReplayDataFromPHP = function(data) {
	this.replay = undefined;
	unquot = data.replace(/&lt;/g, '<').replace(/&gt;/g, '>')
	unquot = unquot.replace(/&quot;/g, '"').replace(/&amp;/g, '&');
	var matches = unquot.match(/[a-zA-Z_]+=.*((?!\n[a-zA-Z_]+=)\n.*)*(?=\n)/gm);
	if (matches) {
		for (var i = 0; i < matches.length; i++) {
			var ep = matches[i].indexOf('=');
			var key = matches[i].substr(0, ep);
			var value = matches[i].substr(ep + 1);
			if (key == 'playback_string') {
				this.loadReplayData(value);
			} else {
				this.parameters[key] = value;
			}
		}
		if (!this.replay) {
			this.errorOut('game_info.php did not feed any playback_string.');
		}
	} else {
		this.errorOut(data);
	}
};
/**
 * Loads a replay string directly.
 * @param {string} data the replay string
 */
Visualizer.prototype.loadReplayData = function(data) {
	this.replay = data;
	window.clearInterval(this.intervalDraw);
	this.intervalDraw = undefined;
	this.timeOffset = undefined;
	this.turn = undefined;
};
/**
 * Places a paragraph with a message in the visualizer dom element.
 * @param {string} text the message text
 * @private
 */
Visualizer.prototype.errorOut = function(text) {
	var errorParagraph = document.createElement("p");
	text = text.split('\n');
	for (var i = 0; i < text.length; i++) {
		errorParagraph.appendChild(document.createTextNode(text[i]));
		errorParagraph.appendChild(document.createElement("br"));
	}
	this.container.appendChild(errorParagraph);
};
/**
 * Places the visualizer in a given container.
 * @param {number} width if defined, the maximum width in pixels
 * @param {number} height if defined, the maximum height in pixels
 */
Visualizer.prototype.attach = function(width, height) {
	this.w = width;
	this.h = height;
	// replace the replay string, by it's parsed counter part
	if (this.replay) {
		try {
			this.replay = new Replay(this.replay);
		} catch (error) {
			this.errorOut('Replay cannot be parsed!\n' + error);
			this.replay = undefined;
		}
	}
	if (this.replay) {
		if (!this.canvas.parentNode) {
			this.container.appendChild(this.canvas);
		}
		// this will fire once in FireFox when a key is held down
		document.onkeydown = function(event) {
			var visualizer = Visualizer.prototype.focused;
			switch(event.keyCode) {
				case Key.SPACE:
					visualizer.play();
					break;
				case Key.LEFT:
					visualizer.stepBw();
					break;
				case Key.RIGHT:
					visualizer.stepFw();
					break;
				case Key.PGUP:
					visualizer.stepBw(10);
					break;
				case Key.PGDOWN:
					visualizer.stepFw(10);
					break;
				case Key.HOME:
					visualizer.gotoStart();
					break;
				case Key.END:
					visualizer.gotoEnd();
					break;
				default:
					var key = String.fromCharCode(event.keyCode);
					switch (key) {
						case 'F':
							visualizer.setFullscreen(!visualizer.config.fullscreen);
							break;
					}
			}
		};
		document.onkeyup = function() {
		};
		// this will fire repeatedly in all browsers
		document.onkeypress = function() {
		};
		window.onresize = function() {
			Visualizer.prototype.focused.resize();
		};
		this.initWaitForImages();
	}
};
/**
 * @private
 */
Visualizer.prototype.initRequestImages = function() {
	this.images.request();
};
Visualizer.prototype.resize = function() {
	var iw = (this.w && !this.config.fullscreen ? this.w : window.innerWidth) - Const.LEFT_PANEL_W;
	var ih = (this.h && !this.config.fullscreen ? this.h : window.innerHeight) - Const.TOP_PANEL_H;
	var newScale = Math.min(10, Math.max(1, Math.min(iw / this.replay.parameters.cols, ih / this.replay.parameters.rows))) | 0;
	if (this.scale != newScale) {
		this.scale = newScale;
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
		this.draw(true);
	}
};
/**
 * @private
 */
Visualizer.prototype.initWaitForImages = function() {
	//if (!this.images.complete()) {
	//	window.setTimeout('visualizer.initWaitForImages()', 50);
	//	return;
	//}
	if (this.canvasBackground === undefined) {
		this.canvasBackground = document.createElement('canvas');
	}
	this.playdir = PlayDir.FORWARD;
	Visualizer.prototype.focused = this;
	this.intervalDraw = window.setInterval('visualizer.draw()', Const.DRAW_INTERVAL);
	this.setFullscreen(this.config.fullscreen);
};
/**
 * @private
 */
Visualizer.prototype.drawColorBar = function(x, y, w, h, values, colors) {
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
	var offsetY = y;
	for (i = 0; i < useValues.length; i++) {
		var amount = scale * useValues[i];
		this.ctx2d.fillStyle = 'rgb(' + colors[i][0] + ', ' + colors[i][1] + ', ' + colors[i][2] + ')';
		this.ctx2d.fillRect(x, offsetY, w, h - offsetY + y);
		offsetY += amount;
	}
	this.ctx2d.textAlign = 'right';
	this.ctx2d.textBaseline = 'top';
	this.ctx2d.font = 'bold 20px Monospace';
	this.ctx2d.fillStyle = 'rgba(0,0,0,0.5)';
	var offsetX = x + w - 2;
	offsetY = y + 2;
	for (i = 0; i < useValues.length; i++) {
		this.ctx2d.fillText(Math.round(values[i]), offsetX, offsetY)
		offsetY += scale * useValues[i];
	}
};
/**
 * @private
 */
Visualizer.prototype.interpolate = function(turn, time, array) {
	if (time == (time | 0)) {
		return this.replay.turns[time + 1][array];
	}
	var delta = turn - time;
	var result = new Array(this.replay.turns[1][array].length);
	for (var i = 0; i < result.length; i++) {
		result[i] = delta * this.replay.turns[turn][array][i] + (1.0 - delta) * this.replay.turns[turn + 1][array][i];
	}
	return result;
}
/**
 * @private
 */
Visualizer.prototype.draw = function(refresh) {
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
			this.turn = turn;
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
	var w = Const.LEFT_PANEL_W / 2;
	var h = this.canvasBackground.height - 20 - Const.TOP_PANEL_H;
	var colors = Const.PLAYER_COLORS;
	if (transition || refresh) {
		//document.getElementById('lblTurn').innerHTML = ((turn > this.replay.turns.length - 2) ? 'end result' : turn + ' / ' + (this.replay.turns.length - 2));
		this.ctx2d.fillStyle = '#000';
		this.ctx2d.fillRect(0, Const.TOP_PANEL_H, 2 * w, this.canvasBackground.height);
		this.ctx2d.fillRect(0, 0, this.canvasBackground.width, Const.TOP_PANEL_H);
		this.ctx2d.textAlign = 'left';
		this.ctx2d.textBaseline = 'top';
		this.ctx2d.font = 'bold 20px sans-serif';
		var x = 0;
		for (i = 0; i < this.replay.players.length; i++) {
			this.ctx2d.fillStyle = 'rgb(' + colors[i][0] + ', ' + colors[i][1] + ', ' + colors[i][2] + ')';
			this.ctx2d.fillText(this.replay.players[i].name, x, 2);
			x += this.ctx2d.measureText(this.replay.players[i].name).width;
			if (i != this.replay.players.length - 1) {
				this.ctx2d.fillStyle = '#888';
				this.ctx2d.fillText(' vs ', x, 2);
				x += this.ctx2d.measureText(' vs ').width;
			}
		}
		this.ctx2d.fillStyle = '#FFF';
		this.ctx2d.textAlign = 'center';
		this.ctx2d.textBaseline = 'middle';
		this.ctx2d.font = 'bold 12px sans-serif';
		this.ctx2d.fillText('ants', w / 2, Const.TOP_PANEL_H + 10);
		this.ctx2d.fillText('scores', 3 * w / 2, Const.TOP_PANEL_H + 10);
	}
	var counts = this.interpolate(turn, time, 'counts');
	this.drawColorBar(2    , Const.TOP_PANEL_H + 20, w - 4, h - 2, counts, Const.PLAYER_COLORS);
	var scores = this.interpolate(turn, time, 'scores');
	this.drawColorBar(2 + w, Const.TOP_PANEL_H + 20, w - 4, h - 2, scores, Const.PLAYER_COLORS);
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
	for (var n = 0; n < drawOrder.length; n++) {
		antObj = drawOrder[n];
		if (true) {
			mx = Math.round(this.scale * antObj.x + Const.LEFT_PANEL_W);
			my = Math.round(this.scale * antObj.y + Const.TOP_PANEL_H);
			this.ctx2d.fillStyle = 'rgba(' + antObj.r + ', ' + antObj.g + ', ' + antObj.b + ', ' + antObj.a + ')';
			this.ctx2d.fillRect(mx, my, this.scale, this.scale);
		} else {
			this.ctx2d.save();
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
	this.timeOld = time;
	//~ var timeB = new Date().getTime();
	//~ document.title = 'Render time: ' + (timeB - timeA) + ' ms';
	//~ return;
};
/**
 * @private
 */
Visualizer.prototype.stepHelper = function(isForward) {
	window.clearInterval(this.intervalDraw);
	this.intervalDraw = undefined;
	if (this.turn === undefined) {
		var time = (new Date().getTime() - this.timeOffset) / Const.TIME_PER_TURN;
		this.turn = (time | 0) + (isForward ? 1 : 2);
	}
};

Visualizer.prototype.stepFw = function(steps) {
	this.stepHelper(true);
	this.turn += (steps === undefined) ? 1 : steps;
	this.playdir = 0;
	if (this.turn > this.replay.turns.length - 1) {
		this.turn = this.replay.turns.length - 1;
	}
	this.draw();
};

Visualizer.prototype.stepBw = function(steps) {
	this.stepHelper(false);
	this.turn -= (steps === undefined) ? 1 : steps;
	this.playdir = 1;
	if (this.turn < 1) {
		this.turn = 1;
	} else if (this.turn > this.replay.turns.length - 2) {
		this.turn = this.replay.turns.length - 2;
	}
	this.draw();
};
	
Visualizer.prototype.gotoStart = function() {
	this.stepHelper(false);
	this.turn = 1;
	this.draw();
};
	
Visualizer.prototype.gotoEnd = function() {
	this.stepHelper(true);
	this.turn = this.replay.turns.length - 1;
	this.draw();
};

Visualizer.prototype.play = function() {
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
	}
	this.draw();
};

Visualizer.prototype.setFullscreen = function(enable) {
	this.config.fullscreen = enable;
	var html = document.getElementsByTagName("html")[0];
	if (enable) {
		this.container.removeChild(this.canvas);
		var tempBody = document.createElement("body");
		tempBody.appendChild(this.canvas);
		this.savedBody = html.replaceChild(tempBody, document.body);
	} else if (this.savedBody) {
		document.body.removeChild(this.canvas);
		this.container.appendChild(this.canvas);
		html.replaceChild(this.savedBody, document.body);
		delete this.savedBody;
	}
	this.resize();
}


/**
 * Random provides functions to generate random numbers in integer ranges.
 */
Random = {
	/**
	 * returns an integer in the range [0..range[
	 * @param {number} range the exclusive limit of the range
	 * @type number
	 */
	range: function(range) {
		return Math.random() * range | 0;
	},
	/**
	 * returns an integer in the range [from..to]
	 * @param {number} from the low value of the range (inclusive)
	 * @param {number} to the high value of the range (inclusive)
	 * @type number
	 */
	fromTo: function(from, to) {
		return from + (Math.random() * (1 + to - from) | 0);
	}
};