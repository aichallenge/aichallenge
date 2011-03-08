/**
 * @fileoverview This is a visualizer for the ant game.
 * @author <a href="mailto:marco.leise@gmx.de">Marco Leise</a>
 * @todo focus button for each player
 * @todo fog of war option
 * @todo scrolling player names if too long; fade to the left and right
 * @todo zoom in to 20x20 squares with animated ants
 * @todo save settings
 * @todo animated single steps if possible (how?)
 * @todo graph showing ants and scores
 * @todo menu items: fullscreen, save, clear, show fog, show attack, show birth,
 *     zoom, toggle graph/score bars, cpu use
 * @todo duplicate sprites that wrap around the map edge
 * @todo improve resource deallocation in case of errors; reset objects
 * @todo keep a minimum size to allow the controls to render
 * @todo setting for cpu usage
 * @todo make loading multiple replays in sequence more reliable
 * @todo set player colors from replay file (awaiting answer)
 * @todo show when a bot crashed (awaiting answer)
 * @todo clickable player names (requires user_id)
 */

/**
 * This global contains constants used throughout the program. I collected them
 * here because NetBeans otherwise clutters the symbol viewer.
 */
Const = {
	LEFT_PANEL_W: 48,        // width of left side panel
	TOP_PANEL_H: 50,         // height of top panel
	BOTTOM_PANEL_H: 64,      // height of bottom panel
	FOOD_COLOR: [ 200, 150, 100 ],
	PLAYER_COLORS: [
		[ 255,   0,   0 ], [   0,   0, 255 ], [   0, 180,   0 ], [ 255, 200,   0 ],
		[ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ],
		[ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ],
		[ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ],
		[ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ],
		[ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ], [ 255, 255, 255 ],
		[ 255, 255, 255 ], [ 255, 255, 255 ]
	],
	COLOR_WATER: '#133',
	COLOR_SAND: '#FFD',
	ZOOM_SCALE: 20,
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


Key = {
	LEFT: 37,
	RIGHT: 39,
	SPACE: 32,
	PGUP: 33,
	PGDOWN: 34,
	HOME: 36,
	END: 35
};


LoadingState = {
	IDLE: 0,
	LOADING: 1,
	CLEANUP: 2
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
LoFiData.prototype.copy = function() {
	var result = new LoFiData();
	for (var i in this) {
		result[i] = this[i];
	}
	return result;
}


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
		return set[goFrom].copy();
	}
	delta = (time - set[goFrom].time) / (set[goFrom + 1].time - set[goFrom].time);
	if (delta == 0) {
		return set[goFrom].copy();
	} else if (delta == 1) {
		return set[goFrom + 1].copy();
	}
	return set[goFrom].interpolate(set[goFrom + 1], delta);
};
Ant.prototype.fade = function(quality, key, pairs) {
	var i, k, av, bv, at, bt, mix;
	var set = quality ? this.hi : this.lo;
	var f = [];
	// create the required frames
	for (i = 0; i < pairs.length; i++) {
		f[i] = this.frameAt(pairs[i].time, quality, true);
		if (pairs[i].value !== undefined) {
			f[i][key] = pairs[i].value;
		}
	}
	// update frames inbetween
	for (i = set.length - 1; i >= 0; i--) {
		if (f[0].time === pairs[0].time) {
			break;
		}
	}
	i++;
	for (k = 0; k < f.length - 1; k++) {
		av = f[k][key];
		bv = f[k + 1][key];
		at = f[k].time;
		bt = f[k + 1].time;
		for (; i < set.length && set[i] != f[k + 1]; i++) {
			mix = (set[i].time - at) / (bt - at);
			set[i][key] = (1 - mix) * av + mix * bv;
		}
	}
};
// animates the ant ([{<time between 0 and 1>, {<attribute to set absolute>, ...}, {<attribute to set relative>, ...}}, ...])
// currently unused
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
			//throw 'A duplicate food item was spawned at (' + data.x + ';' + data.y + ')';
			return id;
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
				//throw 'An existing ant was found at (' + data.x + ';' + data.y + ') found during ant conversion';
				return;
			}
		}
	}
	if (target == null) {
		//throw 'No food was found at (' + data.x + ';' + data.y + ') during ant conversion';
		return;
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
	//throw 'Nothing to kill at (' + data.x + ';' + data.y + ')';
};
Turn.prototype.order = function(data) {
	var moves = false;
	for (var id in this.existing) {
		if (this.existing[id].x == data.x && this.existing[id].y == data.y) {
			if (this.existing[id].player === undefined) {
				//throw 'Attempt to move a food item at (' + data.x + ';' + data.y + ')';
				return;
			}
			this.orders.push({id: this.existing[id].id, direction: data.direction});
			if (moves) {
				//throw 'More than one ant moved at (' + data.x + ';' + data.y + ')';
			}
			moves = true;
		}
	}
	if (moves == 0) {
		//throw 'No ant to move at (' + data.x + ';' + data.y + ')';
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
		return !!window.localStorage;
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
		for (var key in this) {
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
						throw 'Only numbers, booleans and strings can be saved in the config';
				}
				if (prefix) {
					window.localStorage['visualizer.' + key] = prefix + this[key];
				}
			}
		}
		return true;
	}
	return false;
};
/**
 * Tries to load config values from local storage if available. Registred
 * functions on value change will fire.
 */
Config.prototype.load = function() {
	if (this.hasLocalStorage() === true) {
		for (var key in this) {
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
	}
	return false;
};
Config.prototype.clear = function() {
	if (this.hasLocalStorage() === true) {
		window.localStorage.clear();
	}
}


function Replay(replayStr, parameters) {
	this.version = undefined;
	this.players = undefined;
	this.parameters = parameters || {};
	this.walls = undefined;
	this.turns = undefined;

	var row, col, i, k, m, c, f1, f2, collision, spawn, order, conversion;
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
		error.message =	'In line #' + lines.currentNum() + ' "' + lines.current();
		throw error;
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
			f2.x = Math.round(f2.x + order.direction.x);
			f2.y = Math.round(f2.y + order.direction.y);
			antObj.frameAt(t, Quality.LOW, true);
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
				c = collision ? t - 0.75 : t - 0.25;
				antObj.fade(Quality.LOW, 'a', [
					{time: c       , value: undefined},
					{time: c + 0.25, value: 0.0},
					{time: t       , value: 0.0}
				]);
				delete antStates[deathList[k]];
			}
		}
		// food
		for (i in antStates) {
			antObj = antStates[i];
			if (antObj.player === undefined) {
				c = (0.1 * (antObj.lo[0].x + antObj.lo[0].y + t)) % 1;
				f1 = antObj.frameAt(t - c, Quality.LOW, true);
				c = (c - 0.5) < 0 ? c + 0.5 : c - 0.5;
				f2 = antObj.frameAt(t - c, Quality.LOW, true);
				antObj.frameAt(t      , Quality.LOW, true);
				f1.r = Const.FOOD_COLOR[0] + 50;
				f1.g = Const.FOOD_COLOR[1] + 50;
				f1.b = Const.FOOD_COLOR[2] + 50;
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


function ImageInfo(src) {
	this.src = src;
	this.success = undefined;
}


function ImageManager(dataDir, vis, callback) {
	this.dataDir = dataDir;
	this.vis = vis;
	this.callback = callback;
	this.javaApplet = null;
	this.info = [];
	this.images = [];
	this.patterns = [];
	this.error = '';
	this.pending = 0;
}
/**
 * Announces an image that must be loaded. Calling this method after
 * startRequests() results in unexpected behaviour.
 * @see #startRequests
 */
ImageManager.prototype.add = function(source) {
	this.info.push(new ImageInfo(this.dataDir + source));
	this.images.push(null);
	this.patterns.push(null);
};
/**
 * We clean up the state of all images that failed to download in hope that they
 * will succeed next time. This does not apply to the applet version which
 * handles these cases internally.
 */
ImageManager.prototype.cleanUp = function() {
	if (!this.vis.options.java) {
		for (var i = 0; i < this.images.length; i++) {
			if (this.info[i].success === false) {
				this.info[i].success = undefined;
				this.images[i] = null;
				this.pending++;
			}
		}
		this.startRequests();
	}
};
ImageManager.prototype.startRequests = function() {
	var img;
	this.error = '';
	for (var i = 0; i < this.images.length; i++) {
		if (this.info[i].success === undefined && !this.images[i]) {
			if (this.javaApplet) {
				this.images[i] = this.javaApplet.imgRequest(this.info[i].src);
			} else {
				img = new Image();
				this.images[i] = img;
				var that = this;
				img.onload = function() {
					that.imgHandler(this, true);
				};
				img.onerror = function() {
					that.imgHandler(this, false);
				};
				img.onabort = img.onerror;
				img.src = this.info[i].src;
			}
			this.pending++;
		}
	}
	if (this.javaApplet) {
		this.javaApplet.imgWaitFor(this);
	}
};
/**
 * Records the state of an image when the browser has finished loading it. If no
 * more images are pending, the visualizer is signaled.
 * @private
 */
ImageManager.prototype.imgHandler = function(img, success) {
	var i;
	for (i = 0; i < this.images.length; i++) {
		if (this.images[i].src === img.src) break;
	}
	if (!success) {
		if (this.error) this.error += '\n';
		this.error += this.info[i].src + ' did not load.';
	}
	this.info[i].success = success;
	if (--this.pending == 0) {
		this.vis[this.callback](this.error);
	}
};
/**
 * Sets the pattern of an image to a CanvasPattern, wich can be used as
 * fillStyle in drawing operations to create a repeated tile texture.
 */
ImageManager.prototype.pattern = function(idx, ctx, repeat) {
	if (!this.patterns[idx]) {
		this.patterns[idx] = ctx.createPattern(this.images[idx], repeat);
	}
	ctx.fillStyle = this.patterns[idx];
};
/**
 * Sets the pattern of an image to a set of colorized copies of itself.
 */
ImageManager.prototype.colorize = function(idx, colors) {
	var obj = {};
	this.vis.createCanvas(obj);
	this.patterns[idx] = obj.canvas;
	obj.canvas.width = this.images[idx].width * colors.length;
	obj.canvas.height = this.images[idx].height;
	var ctx = obj.ctx;
	ctx.fillStyle = ctx.createPattern(this.images[idx], 'repeat');
	ctx.fillRect(0, 0, obj.canvas.width, obj.canvas.height);
	if (this.javaApplet) {
		// technically this is not neccesary, but it in praxis it would take
		// ages to manipulate pixels through LiveConnect
		this.javaApplet.imageOps.colorize(ctx, colors);
	} else {
		var data = ctx.getImageData(0, 0, obj.canvas.width, obj.canvas.height);
		var d = data.data;
		var ox = 0;
		var dx = 4 * this.images[idx].width;
		for (var i = 0; i < colors.length; i++) {
			var c = colors[i];
			for (var y = 0; y < 4 * data.width * data.height; y += 4 * data.width) {
				for (var p = y + ox; p < y + ox + dx; p += 4) {
					if (d[p] === d[p+1] && d[p] === d[p+2]) {
						// only gray pixels
						d[p+0] = (d[p+0] + c[0]) >> 1;
						d[p+1] = (d[p+1] + c[1]) >> 1;
						d[p+2] = (d[p+2] + c[2]) >> 1;
					}
				}
			}
			ox += dx;
		}
		ctx.putImageData(data, 0, 0);
	}
};


/**
 * @class The main 'application' object that provides all necessary methods for
 *     the use in a web page.
 * @param {Node} container the html element, that the visualizer will embed into
 * @param {String} dataDir This relative path to the visualizer data files. You
 *     will get an error message if you forget the tailing '/'.
 * @param {String} codebase The java applet will be loaded from this location.
 * @param {Number} w an optional maximum width or undefined
 * @param {Number} h an optional maximum height or undefined
 * @param {Boolean} java if set to true or false, this overrides the
 *     auto-detection of the Java mode
 */
function Visualizer(container, dataDir, codebase, w, h, java) {
	/**
	 * any generated DOM elements will be placed here
	 * @private
	 */
	this.container = container;
	/**
	 * contains the backdrop for the map
	 * @private
	 */
	this.map = {};
	/**
	 * Caches the graphics of the map border
	 * @private
	 */
	this.border = {};
	/**
	 * array of precomputed turn data
	 * @private
	 */
	this.turns = undefined;
	/**
	 * usable width for the visualizer
	 * @private
	 */
	this.w = w;
	/**
	 * usable height for the visualizer
	 * @private
	 */
	this.h = h;
	/**
	 * locations of elements on the screen
	 * @private
	 */
	this.loc = {};
	/**
	 * size of an ant in pixels
	 * @private
	 */
	this.scale = undefined;
	/**
	 * manages playback commands and timing
	 * @private
	 */
	this.director = new Director(this);
	/**
	 * presistable configuration values
	 * @private
	 */
	this.config = new Config();
	/**
	 * Options from URL GET parameters or the constructor arguments
	 * @private
	 */
	this.options = {};
	this.options.java = java;
	this.options.data_dir = dataDir;
	this.options.codebase = codebase;
	// read URL parameters and store them in the parameters object
	var equalPos, value, key, i;
	var parameters = window.location.href.split('?');
	if (parameters.length > 1) {
		parameters = parameters[1].split('#')[0].split('&');
		for (i = 0; i < parameters.length; i++) {
			equalPos = parameters[i].indexOf('=');
			key = parameters[i].substr(0, equalPos);
			value = parameters[i].substr(equalPos + 1);
			if (key === 'java' || key === 'debug') value = new Boolean(value);
			this.options[key] = value;
		}
	}
	/**
	 * buttons
	 * @private
	 */
	this.btnMgr = new ButtonManager(this);
	/**
	 * @private
	 */
	this.log = document.createElement('div');
	// print some information text
	var text = 'Loading visualizer...';
	text += '<table>';
	for (key in this.options) {
		value = this.options[key];
		text += '<tr><td>-&nbsp;</td><td>' + key + '&nbsp;&nbsp;</td><td><b>' + value + '&nbsp;&nbsp;</b></td><td><i>';
		if (key == "java") {
			text += '(Display method: ';
			if (value === undefined) {
				this.options.java = !document.createElement('canvas').getContext;
				text += (this.options.java ? 'Java Applet [autodetected]' : 'HTML Canvas [autodetected]') + ')';
			} else {
				text += (value ? 'Java Applet' : 'HTML Canvas') + ')';
			}
		} else if (key == "data_dir") {
			text += '(Image directory)';
		} else if (key == "codebase") {
			text += '(Java codebase)';
		}
		text += '</i></td></tr>';
	}
	text += '</table>';
	while(this.container.hasChildNodes()){
		this.container.removeChild(this.container.lastChild);
	}
	this.log.innerHTML = text;
	this.container.appendChild(this.log);
	/**
	 * images used by the visualizer
	 * @private
	 */
	this.imgMgr = new ImageManager((dataDir || '') + 'img/', this, 'completedImages');
	this.imgMgr.add('wood.jpg');
	this.imgMgr.add('playback.png');
	this.imgMgr.add('fog.png');
	// state information that must be reset on error/reload
	/**
	 * @private
	 */
	this.replay = undefined;
	/**
	 * the main canvas
	 * @private
	 */
	this.main = {};
	/**
	 * @private
	 */
	this.loading = LoadingState.IDLE;
	// If we use a Java applet, we have to delay the image requests until we
	// can pass the image URLs over to it.
	if (!this.options.java) this.imgMgr.startRequests();
}
/**
 * @private
 */
Visualizer.prototype.progress = function(log, func) {
	if (this.loading !== LoadingState.LOADING) return;
	var vis = this;
	if (log) this.logOut(log);
	window.setTimeout(function() {
		try {
			func();
		} catch (error) {
			// (for Firefox Java errors:) if error is just a string, wrap it into an object
			if (typeof error == 'string') error = { message: error };
			var msg = '';
			for(var key in error) {
				msg += '<p><u><b>Error ' + key + ':</b></u>\n' + error[key] + '</p>';
			}
			vis.errorOut(msg);
			var selectedPosX = 0;
			var selectedPosY = 0;
			var obj = vis.log;
			if (vis.log.offsetParent) do {
				selectedPosX += obj.offsetLeft;
				selectedPosY += obj.offsetTop;
			} while ((obj = obj.offsetParent));
			window.scrollTo(selectedPosX, selectedPosY);
		}
	}, 50);
};
/**
 * Places a paragraph with a message in the visualizer dom element.
 * @param {string} text the message text
 * @private
 */
Visualizer.prototype.logOut = function(text) {
	text = text.replace(/\n/g, '<br>');
	this.log.innerHTML += text + '<br>';
};
/**
 * Stops loading, cleans up the instance and calls logOut with the text in red.
 * @param {string} text the error message text
 * @private
 */
Visualizer.prototype.errorOut = function(text) {
	this.logOut('<font color="red">' + text + '</font>');
	this.cleanUp();
};
/**
 * @private
 */
Visualizer.prototype.cleanUp = function() {
	this.loading = LoadingState.CLEANUP;
	this.imgMgr.cleanUp();
	this.director.cleanUp();
	if (this.replay && this.replay instanceof XMLHttpRequest) this.replay.abort();
	this.replay = undefined;
	if (this.main.element && !this.options.java) {
		if (this.container.firstChild === this.main.element) {
			this.container.removeChild(this.main.element);
		}
	}
	document.onkeydown = null;
	document.onkeyup = null;
	document.onkeypress = null;
	window.onresize = null;
	this.log.style.display = 'block';
};
Visualizer.prototype.preload = function() {
	if (this.loading !== LoadingState.IDLE) return true;
	while (this.log.firstElement !== this.log.lastElement) {
		this.log.removeElement(this.log.lastElement);
	}
	this.cleanUp();
	this.loading = LoadingState.LOADING;
	return false;
};
/**
 * Loads a replay file located on the same server using a XMLHttpRequest.
 * @param {string} file the relative file name
 */
Visualizer.prototype.loadReplayDataFromURI = function(file) {
	if (this.preload()) return;
	var vis = this;
	this.progress('Fetching replay from: <i>' + file + '</i>...', function() {
		var vis = this;
		this.replay = new XMLHttpRequest();
		this.replay.onreadystatechange = function() {
			if (vis.replay.readyState === 4) {
				if (vis.loading === LoadingState.LOADING) {
					if (vis.replay.status === 200) {
						vis.replay = vis.replay.responseText;
						vis.loadParseReplay();
					} else {
						vis.errorOut('Status ' + p.status + ': ' + p.statusText);
					}
				}
			}
		};
		this.replay.open("GET", file);
		this.replay.send(null);
		this.loadCanvas(true);
	});
};
/**
 * Loads a replay file through the php site. This data has html special
 * characters encoded and has a general structure of key=value.
 * @param {string} data the encoded replay data
 */
Visualizer.prototype.loadReplayDataFromPHP = function(data) {
	if (this.preload()) return;
	var vis = this;
	this.progress('Reading replay passed in from php...', function() {
		var parameters = {};
		var unquot = data.replace(/&lt;/g, '<').replace(/&gt;/g, '>')
		unquot = unquot.replace(/&quot;/g, '"').replace(/&amp;/g, '&');
		var matches = unquot.match(/[a-zA-Z_]+=.*((?!\n[a-zA-Z_]+=)\n.*)*(?=\n)/gm);
		var replay = undefined;
		if (matches) {
			for (var i = 0; i < matches.length; i++) {
				var ep = matches[i].indexOf('=');
				var key = matches[i].substr(0, ep);
				var value = matches[i].substr(ep + 1);
				if (key == 'playback_string') {
					replay = value;
				} else {
					parameters[key] = value;
				}
			}
			if (!replay) {
				vis.errorOut('game_info.php did not feed any playback_string.');
			} else {
				vis.replay = {replay: replay, parameters: parameters};
				vis.loadCanvas(true);
			}
		} else if (data) {
			vis.errorOut(data);
		} else {
			vis.errorOut('no data');
		}
	});
};
/**
 * Loads a replay string directly.
 * @param {string} data the replay string
 */
Visualizer.prototype.loadReplayData = function(data) {
	if (this.preload()) return;
	this.replay = data;
	this.loadCanvas(true);
};
/**
 * @private
 */
Visualizer.prototype.loadParseReplay = function() {
	if (this.replay && this.replay instanceof Replay) return;
	var vis = this;
	this.progress('Parsing the replay...', function() {
		if (!vis.replay) {
			throw new Error('Replay is undefined.');
		} else if (typeof vis.replay == 'string') { // string only
			vis.replay = new Replay(vis.replay);
		} else if (vis.replay instanceof XMLHttpRequest) {
			throw new Error('Attempted to manualy trigger the replay parsing process while waiting for the download.');
		} else if (vis.replay instanceof Object) { // string + param
			vis.replay = new Replay(vis.replay.replay, vis.replay.parameters);
		} else {
			throw new Error('Something unknown is in the replay variable: ' + vis.replay);
		}
		vis.tryStart();
	});
};
/**
 * Creates a canvas element
 * @private
 */
Visualizer.prototype.loadCanvas = function(prompt) {
	var vis = this;
	this.progress(prompt ? 'Creating canvas...' : undefined, function() {
		var size = vis.calculateCanvasSize();
		if (!vis.main.element) {
			if (vis.options.java) {
				var token = appletManager.add(vis);
				var e = document.createElement('applet');
				vis.main.element = e;
				e.setAttribute('codebase', vis.options.codebase);
				e.setAttribute('code', 'com.aicontest.visualizer.CanvasApplet');
				e.setAttribute('width', size.width);
				e.setAttribute('height', size.height);
				e.setAttribute('mayscript', 'true');
				var param = function(name, value) {
					var p = document.createElement('param');
					p.setAttribute('name', name);
					p.setAttribute('value', value);
					e.appendChild(p);
					e.setAttribute(name, value);
				}
				if (vis.options.debug) {
					param('separate_jvm', 'true');
					param('classloader_cache', 'false');
					param('debug', true);
				}
				param('token', token);
				vis.container.insertBefore(e, vis.log);
				// wait for the applet to call back
				return;
			} else {
				vis.main.element = document.createElement('canvas');
				vis.main.canvas = vis.main.element;
			}
		}
		// from here on I can handle both canvases the same
		vis.main.ctx = vis.main.canvas.getContext('2d');
		e = vis.main.element;
//		if (e.width != size.width || e.height != size.height) {
//			e.width = size.width;
//			e.height = size.height;
//		}
		if (vis.container.firstChild !== e) {
			vis.container.insertBefore(e, vis.log);
		}
		vis.createCanvas(vis.map);
		vis.createCanvas(vis.border);
		if (!vis.btnMgr.groups.playback) {
			with (vis.btnMgr.addGroup('playback', vis.imgMgr.images[1], ButtonGroup.HORIZONTAL, ButtonGroup.MODE_NORMAL, 2)) {
				addButton(3, function() {vis.director.gotoTick(0)});
				addSpace(32);
				addButton(5, function() {vis.director.gotoTick(Math.ceil(vis.director.position) - 1)});
				//drawImage(this.imgMgr.images[1], 0 * 64, 0, 64, 64, x + 2.5 * 64, y, 64, 64);
				addSpace(64);
				addButton(4, function() {vis.director.playStop()});
				//drawImage(this.imgMgr.images[1], 1 * 64, 0, 64, 64, x + 4.5 * 64, y, 64, 64);
				addSpace(64);
				addButton(6, function() {vis.director.gotoTick(Math.floor(vis.director.position) + 1)});
				addSpace(32);
				addButton(2, function() {vis.director.gotoTick(vis.director.duration)});
			}
		}
		vis.tryStart();
	});
};
/**
 * Called by the AppletManager when the applet is initialized
 */
Visualizer.prototype.initializedApplet = function() {
	this.main.canvas = this.main.element.getMainCanvas();
	this.imgMgr.javaApplet = this.main.element;
	this.imgMgr.startRequests();
	this.loadCanvas(false);
};
/**
 * Called by the ImageManager when no more images are loading
 */
Visualizer.prototype.completedImages = function(error) {
	if (error) {
		this.errorOut(error);
	} else {
		this.tryStart();
	}
};
/**
 * Checks if we have a drawing context (canvas/applet), the images and the
 * replay. If all components are loaded it starts playback.
 */
Visualizer.prototype.tryStart = function() {
	// we need to parse the replay, unless it has been parsed by the
	// XmlHttpRequest callback
	if (this.replay && this.replay instanceof Replay) {
		if (this.main.ctx && !this.imgMgr.error && !this.imgMgr.pending) {
			var vis = this;
			// generate fog images
			var colors = [[255, 255, 255]].concat(Const.PLAYER_COLORS.slice(0, this.replay.players.length));
			this.imgMgr.colorize(2, colors);
			var bg = this.btnMgr.addGroup('fog', this.imgMgr.patterns[2], ButtonGroup.VERTICAL, ButtonGroup.MODE_HIDDEN, 2);
			bg.y = Const.TOP_PANEL_H;
			for (var i = 0; i < colors.length; i++) {
				bg.addButton(i, function() { /*vis.showFog(i);*/ });
			}
			// try to make the replays play 1 minute, but the turns take no more than a second
			this.director.duration = this.replay.turns.length - 2;
			this.director.defaultSpeed = Math.max(this.director.duration / 60, 1);
			this.director.onstate = function() {
				var btn = vis.btnMgr.groups.playback.buttons[4];
				btn.offset = (vis.director.playing() ? 7 : 4) * vis.imgMgr.images[1].height;
				if (btn === vis.btnMgr.nailed) {
					vis.btnMgr.nailed = null;
				}
				btn.mouseUp();
			};
			// this will fire once in FireFox when a key is held down
			document.onkeydown = function(event) {
				if (!event) {
					// IE doesn't pass this as an argument
					event = window.event;
				}
				vis.keyPressed(event.keyCode);
			};
			if (this.options.java) {
				this.main.element.setInputHandler(this);
			} else {
				// setup mouse handlers
				this.main.element.onmousemove = function(event) {
					var mx = 0;
					var my = 0;
					var obj = this;
					if (this.offsetParent) do {
						mx += obj.offsetLeft;
						my += obj.offsetTop;
					} while ((obj = obj.offsetParent));
					with (event || window.event) {
						mx = clientX - mx + ((window.scrollX === undefined) ? (document.body.parentNode.scrollLeft !== undefined) ? document.body.parentNode.scrollLeft : document.body.scrollLeft : window.scrollX);
						my = clientY - my + ((window.scrollY === undefined) ? (document.body.parentNode.scrollTop !== undefined) ? document.body.parentNode.scrollTop : document.body.scrollTop : window.scrollY);
					}
					vis.mouseMoved(mx, my);
				};
				this.main.element.onmouseout = function() {
					vis.mouseExited();
				};
				this.main.element.onmousedown = function() {
					vis.mousePressed();
				};
				this.main.element.onmouseup = function() {
					vis.mouseReleased();
				};
			}
			window.onresize = function() {
				vis.resize();
			};
			Visualizer.prototype.focused = this;
			this.setFullscreen(this.config.fullscreen);
			this.director.play();
			this.log.style.display = 'none';
			this.loading = LoadingState.IDLE;
		}
	} else {
		this.loadParseReplay();
	}
};
Visualizer.prototype.calculateCanvasSize = function() {
	var result = {};
	if (typeof(window.innerWidth) == 'number' ) {
		//Non-IE
		result.width = window.innerWidth;
		result.height = window.innerHeight;
	} else if (document.documentElement && (document.documentElement.clientWidth || document.documentElement.clientHeight)) {
		//IE 6+ in 'standards compliant mode'
		result.width = document.documentElement.clientWidth;
		result.height = document.documentElement.clientHeight;
	} else if (document.body && (document.body.clientWidth || document.body.clientHeight)) {
		//IE 4 compatible
		result.width = document.body.clientWidth;
		result.height = document.body.clientHeight;
	}
	result.width = (this.w && !this.config.fullscreen ? this.w : result.width);
	result.height = (this.h && !this.config.fullscreen ? this.h : result.height - 4);
	return result;
};
Visualizer.prototype.createCanvas = function(obj) {
	if (!obj.canvas) {
		if (this.options.java) {
			obj.canvas = this.main.element.createCanvas();
		} else {
			obj.canvas = document.createElement('canvas');
		}
	}
	if (!obj.ctx) {
			obj.ctx = obj.canvas.getContext('2d');
	}
};
Visualizer.prototype.setFullscreen = function(enable) {
	if (!this.options.java) {
		var html = document.getElementsByTagName("html")[0];
		this.config.fullscreen = enable;
		if (enable) {
			this.container.removeChild(this.main.element);
			var tempBody = document.createElement("body");
			tempBody.appendChild(this.main.element);
			this.savedBody = html.replaceChild(tempBody, document.body);
		} else if (this.savedBody) {
			document.body.removeChild(this.main.element);
			this.container.appendChild(this.main.element);
			html.replaceChild(this.savedBody, document.body);
			delete this.savedBody;
		}
	}
	this.resize(true);
};
Visualizer.prototype.resize = function(forced) {
	var olds = {
		width: this.main.element.getAttribute('width'),
		height: this.main.element.getAttribute('height')
	};
	var news = this.calculateCanvasSize();
	var resizing = news.width != olds.width || news.height != olds.height;
	if (resizing || forced) {
		if (resizing) {
			this.main.element.setAttribute('width', news.width);
			this.main.element.setAttribute('height', news.height);
			if (this.options.java) {
				this.main.element.setSize(news.width, news.height);
			}
		}
		this.loc.vis = {
			w: news.width - Const.LEFT_PANEL_W,
			h: news.height - Const.TOP_PANEL_H - Const.BOTTOM_PANEL_H,
			x: Const.LEFT_PANEL_W,
			y: Const.TOP_PANEL_H
		};
		this.scale = Math.min(10, Math.max(1, Math.min(
			(this.loc.vis.w - 2 * Const.ZOOM_SCALE) / (this.replay.parameters.cols),
			(this.loc.vis.h - 2 * Const.ZOOM_SCALE) / (this.replay.parameters.rows)
		))) | 0;
		this.loc.map = {
			w: this.scale * (this.replay.parameters.cols),
			h: this.scale * (this.replay.parameters.rows)
		};
		this.loc.map.x = ((this.loc.vis.w - this.loc.map.w) / 2 + this.loc.vis.x) | 0;
		this.loc.map.y = ((this.loc.vis.h - this.loc.map.h) / 2 + this.loc.vis.y) | 0;
		this.border.canvas.width = this.loc.map.w + 2 * Const.ZOOM_SCALE;
		this.border.canvas.height = this.loc.map.h + 2 * Const.ZOOM_SCALE;
		this.renderBorder();
		this.map.canvas.width = this.loc.map.w;
		this.map.canvas.height = this.loc.map.h;
		this.renderMap();
		with (this.btnMgr.groups) {
			playback.x = ((news.width - 8 * 64) / 2) | 0;
			playback.y = this.loc.vis.y + this.loc.vis.h;
			fog.y = (this.loc.vis.y + (this.loc.vis.h - fog.h) / 2) | 0
		}
		// redraw everything
		this.btnMgr.draw();
		// draw player names and captions
		var colors = Const.PLAYER_COLORS;
		this.main.ctx.textAlign = 'left';
		this.main.ctx.textBaseline = 'top';
		this.main.ctx.font = 'bold 20px Arial';
		var x = 0;
		for (var i = 0; i < this.replay.players.length; i++) {
			this.main.ctx.fillStyle = 'rgb(' + colors[i][0] + ', ' + colors[i][1] + ', ' + colors[i][2] + ')';
			this.main.ctx.fillText(this.replay.players[i].name, x, 2);
			x += this.main.ctx.measureText(this.replay.players[i].name).width;
			if (i != this.replay.players.length - 1) {
				this.main.ctx.fillStyle = '#888';
				this.main.ctx.fillText(' vs ', x, 2);
				x += this.main.ctx.measureText(' vs ').width;
			}
		}
		var w = this.main.canvas.width;
		this.main.ctx.fillStyle = '#000';
		this.main.ctx.textAlign = 'center';
		this.main.ctx.textBaseline = 'middle';
		this.main.ctx.font = 'bold 12px Arial';
		this.main.ctx.fillText('ants'  , 30          , Const.TOP_PANEL_H - 10);
		this.main.ctx.fillText('scores', 30 + 0.5 * w, Const.TOP_PANEL_H - 10);
		this.director.draw();
	}
};
/**
 * @private
 */
Visualizer.prototype.renderMap = function() {
	var ctx = this.map.ctx;
	ctx.fillStyle = Const.COLOR_SAND;
	ctx.fillRect(0, 0, this.loc.map.w, this.loc.map.h);
	ctx.fillStyle = Const.COLOR_WATER;
	for (var row = 0; row < this.replay.parameters.rows; row++) {
		var start = undefined;
		for (var col = 0; col < this.replay.parameters.cols; col++) {
			var isWall = this.replay.walls[row][col];
			if (start === undefined && isWall) {
				start = col;
			} else if (start !== undefined && !isWall) {
				ctx.fillRect(this.scale * start, this.scale * row, this.scale * (col - start), this.scale);
				start = undefined;
			}
		}
		if (start !== undefined) {
			ctx.fillRect(this.scale * start, this.scale * row, this.scale * (col - start), this.scale);
		}
	}
};
/**
 * @private
 */
Visualizer.prototype.renderBorder = function() {
	var ctx = this.border.ctx;
	with(Const) {
		ctx.save();
			this.imgMgr.pattern(0, ctx, 'repeat');
			ctx.translate(ZOOM_SCALE, ZOOM_SCALE);
			with (this.loc.map) {
				ctx.save();
					ctx.beginPath();
					ctx.moveTo(0, 0);
					ctx.lineTo(w, 0);
					ctx.lineTo(w + ZOOM_SCALE, -ZOOM_SCALE);
					ctx.lineTo(-ZOOM_SCALE, -ZOOM_SCALE);
					ctx.closePath();
					ctx.fill();
				ctx.restore();
				ctx.save();
					ctx.translate(0, h);
					ctx.beginPath();
					ctx.moveTo(0, 0);
					ctx.lineTo(w, 0);
					ctx.lineTo(w + ZOOM_SCALE, +ZOOM_SCALE);
					ctx.lineTo(-ZOOM_SCALE, +ZOOM_SCALE);
					ctx.closePath();
					ctx.fill();
				ctx.restore();
				ctx.rotate(0.5 * Math.PI);
				ctx.save();
					ctx.beginPath();
					ctx.moveTo(0, 0);
					ctx.lineTo(h, 0);
					ctx.lineTo(h + ZOOM_SCALE, ZOOM_SCALE);
					ctx.lineTo(-ZOOM_SCALE, ZOOM_SCALE);
					ctx.closePath();
					ctx.fill();
				ctx.restore();
				ctx.save();
					ctx.translate(0, -w);
					ctx.beginPath();
					ctx.moveTo(0, 0);
					ctx.lineTo(h, 0);
					ctx.lineTo(h + ZOOM_SCALE, -ZOOM_SCALE);
					ctx.lineTo(-ZOOM_SCALE, -ZOOM_SCALE);
					ctx.closePath();
					ctx.fill();
			   ctx.restore();
			}
		ctx.restore();
	}
};
/**
 * @private
 */
Visualizer.prototype.drawColorBar = function(x, y, w, h, values, colors) {
	var sum = 0;
	this.main.ctx.save();
	this.main.ctx.beginPath();
	this.main.ctx.rect(x, y, w, h);
	this.main.ctx.clip();
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
	var scale = w / sum;
	var offsetX = x;
	for (i = 0; i < useValues.length; i++) {
		var amount = scale * useValues[i];
		this.main.ctx.fillStyle = 'rgb(' + colors[i][0] + ', ' + colors[i][1] + ', ' + colors[i][2] + ')';
		this.main.ctx.fillRect(offsetX, y, w - offsetX + x, h);
		offsetX += amount;
	}
	this.main.ctx.textAlign = 'left';
	this.main.ctx.textBaseline = 'middle';
	this.main.ctx.font = 'bold 16px Monospace';
	this.main.ctx.fillStyle = 'rgba(0,0,0,0.5)';
	var offsetY = y + 0.5 * h;
	offsetX = x + 2;
	for (i = 0; i < useValues.length; i++) {
		if (useValues[i] != 0) {
			this.main.ctx.fillText(Math.round(values[i]), offsetX, offsetY);
		}
		offsetX += scale * useValues[i];
	}
	this.main.ctx.restore();
};
/**
 * @private
 */
Visualizer.prototype.correctCoords = function(obj, w, h) {
	obj.x -= Math.floor(obj.x / w) * w;
	obj.y -= Math.floor(obj.y / h) * h;
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
};
/**
 * @private
 */
Visualizer.prototype.draw = function(time, tick) {
	var cx, cy, mx, my;
	var drawOrder = [];
	var turn = (time | 0) + 1;
	// draw the map background
	this.main.ctx.fillStyle = '#ff3';
	this.main.ctx.drawImage(this.map.canvas, this.loc.map.x, this.loc.map.y);
	var w = this.main.canvas.width;
	if (tick !== undefined) {
		//document.getElementById('lblTurn').innerHTML = ((turn > this.replay.turns.length - 2) ? 'end result' : turn + ' / ' + (this.replay.turns.length - 2));
	}
	var counts = this.interpolate(turn, time, 'counts');
	this.drawColorBar(60,           Const.TOP_PANEL_H - 20, 0.5 * w - 60, 20, counts, Const.PLAYER_COLORS);
	var scores = this.interpolate(turn, time, 'scores');
	this.drawColorBar(60 + 0.5 * w, Const.TOP_PANEL_H - 20, 0.5 * w - 60, 20, scores, Const.PLAYER_COLORS);
	for (var i = 0; i < this.replay.turns[turn].ants.length; i++) {
		var antObj = this.replay.turns[turn].ants[i].interpolate(time, Quality.LOW);
		this.correctCoords(antObj, this.replay.parameters.cols, this.replay.parameters.rows);
		drawOrder.push(antObj);
	}
	for (var n = 0; n < drawOrder.length; n++) {
		antObj = drawOrder[n];
		if (this.config.zoom) {
			this.main.ctx.save();
			this.main.ctx.globalAlpha = antObj.alpha;
			cx = Const.ZOOM_SCALE * (antObj.x + 0.5);
			cy = Const.ZOOM_SCALE * (antObj.y + 0.5);
			this.main.ctx.translate(cx, cy);
			this.main.ctx.rotate(antObj.angle + Math.sin(20 * time) * antObj.jitter);
			this.main.ctx.drawImage(this.imgMgr.ants[antObj.type], -10, -10);
			this.main.ctx.restore();
			mx = cx + 3 * Math.tan(2 * (Math.random() - 0.5));
			my = cy + 3 * Math.tan(2 * (Math.random() - 0.5));
			if (antObj.alpha == 1) {
				var sin = -Math.sin(antObj.angle);
				var cos = +Math.cos(antObj.angle);
				this.ctxMap.moveTo(mx - sin, my - cos);
				this.ctxMap.lineTo(mx + sin, my + cos);
			}
		} else {
			mx = Math.round(this.scale * antObj.x) + this.loc.map.x;
			my = Math.round(this.scale * antObj.y) + this.loc.map.y;
			this.main.ctx.fillStyle = 'rgba(' + antObj.r + ', ' + antObj.g + ', ' + antObj.b + ', ' + antObj.a + ')';
			this.main.ctx.fillRect(mx, my, this.scale, this.scale);
		}
	}
	// draw border over ants, that moved out of the map
	this.main.ctx.drawImage(this.border.canvas, this.loc.map.x - Const.ZOOM_SCALE, this.loc.map.y - Const.ZOOM_SCALE);
	if (this.options.java) {
		this.main.element.repaint();
	}
};
Visualizer.prototype.mouseMoved = function(mx, my) {
	this.btnMgr.mouseMove(mx, my);
};
Visualizer.prototype.mousePressed = function() {
	this.btnMgr.mouseDown();
};
Visualizer.prototype.mouseReleased = function() {
	this.btnMgr.mouseUp();
};
Visualizer.prototype.mouseExited = function() {
	this.btnMgr.mouseMove(-1, -1);
};
Visualizer.prototype.mouseEntered = function(mx, my, down) {
	if (!down) this.btnMgr.mouseUp();
	this.btnMgr.mouseMove(mx, my);
};
Visualizer.prototype.keyPressed = function(key) {
	with (this.director) {
		switch(key) {
			case Key.SPACE:
				playStop();
				break;
			case Key.LEFT:
				gotoTick(Math.ceil(position) - 1);
				break;
			case Key.RIGHT:
				gotoTick(Math.floor(position) + 1);
				break;
			case Key.PGUP:
				gotoTick(Math.ceil(position) - 10);
				break;
			case Key.PGDOWN:
				gotoTick(Math.floor(position) + 10);
				break;
			case Key.HOME:
				gotoTick(0);
				break;
			case Key.END:
				gotoTick(duration);
				break;
			default:
				switch (String.fromCharCode(key)) {
					case 'F':
						visualizer.setFullscreen(!visualizer.config.fullscreen);
						break;
				}
		}
	}
};
Visualizer.prototype.keyReleased = function(key) {
};


/**
 * @class The director is supposed to keep track of playback speed and position.
 */
function Director(vis) {
	this.speed = 0;
	this.position = 0;
	this.lastTime = undefined;
	this.vis = vis;
	this.duration = 0;
	this.defaultSpeed = 1;
	this.cpu = 0.5;
	this.onstate = undefined;
	this.timeout = undefined;
}
/**
 * When the director is in playback mode it has a lastTime. This is just a
 * convenience method of querying the playback mode.
 * @type boolean
 */
Director.prototype.playing = function() {
	return this.speed !== 0;
};
Director.prototype.playStop = function() {
	this.playing() ? this.stop() : this.play();
};
Director.prototype.play = function() {
	if (!this.playing()) {
		if (this.position == this.duration) {
			this.position = 0;
		}
		this.speed = this.defaultSpeed;
		if (this.onstate) this.onstate();
		this.loop(false);
	}
};
Director.prototype.stop = function() {
	if (this.playing()) {
		this.speed = 0;
		this.lastTime = undefined;
		if (this.onstate) this.onstate();
	}
};
Director.prototype.gotoTick = function(tick) {
	this.stop();
	if (tick < 0) {
		tick = 0;
	} else if (tick > this.duration) {
		tick = this.duration;
	}
	if (this.position != tick) {
		var oldTick = this.position | 0;
		this.position = tick;
		this.vis.draw(this.position, oldTick !== tick);
	}
};
Director.prototype.loop = function() {
	if (this.speed === 0) {
		return;
	}
	var lastTime = this.lastTime;
	this.lastTime = new Date().getTime();
	var goOn = true;
	var oldTurn = this.position | 0;
	if (lastTime === undefined) {
		oldTurn = -1;
	} else {
		this.position += (this.lastTime - lastTime) * this.speed * 0.001;
	}
	if (this.position <= 0 && this.speed < 0) {
		this.position = 0;
		goOn = false;
		this.stop();
	} else if (this.position >= this.duration && this.speed > 0) {
		this.position = this.duration;
		goOn = false;
		this.stop();
	}
	this.vis.draw(this.position, (oldTurn != (this.position | 0)) ? this.position | 0 : undefined);
	if (goOn) {
		var that = this;
		var cpuTime = new Date().getTime() - this.lastTime;
		var delay = (this.cpu <= 0 || this.cpu > 1) ? 0 : Math.ceil(cpuTime / this.cpu - cpuTime);
		this.timeout = window.setTimeout(function() {that.loop(true)}, delay);
	}
};
Director.prototype.cleanUp = function() {
	window.clearTimeout(this.timeout);
	this.stop();
	this.position = 0;
};
/**
 * Causes the visualizer to draw the current game state. For performance reasons
 * this is not done when the visualizer is already playing back anyway.
 */
Director.prototype.draw = function() {
	if (!this.playing()) {
		this.vis.draw(this.position, this.position | 0);
	}
};
/**
 * When an applet goes fullscreen it is detached and reinitialized. We need to
 * stop the animation until it is available again.
 */
Director.prototype.freeze = function() {
	window.clearTimeout(this.timeout);
};


function Button(group, offset, delta, action) {
	this.group = group;
	this.offset = offset;
	this.delta = delta;
	this.action = action;
	this.hover = false;
	this.down = false;
}
Button.prototype.mouseDown = function() {
	switch (this.group.mode) {
		case ButtonGroup.MODE_RADIO:
			if (this.down) return;
			var btns = this.group.buttons;
			for (var i = 0; i < btns.length; i++) {
				if (btns[i].down) {
					btns[i].down = false;
					btns[i].draw();
				}
			}
		case ButtonGroup.MODE_NORMAL:
			this.down = true;
			this.draw();
			break;
	}
};
Button.prototype.mouseUp = function() {
	switch (this.group.mode) {
		case ButtonGroup.MODE_NORMAL:
			this.down = false;
			this.draw();
			break;
	}
};
Button.prototype.draw = function() {
	var ctx = this.group.manager.vis.main.ctx;
	with (this.group) {
		var ix = x + (vertical ? 0 : this.delta);
		var iy = y + (vertical ? this.delta : 0);
		var n = 1;
		var r = 0.2 * this.group.size;
		var d = 0.5 * Math.PI;
		ctx.save();
		ctx.translate(ix, iy);
		ctx.clearRect(0, 0, size, size);
		ctx.beginPath();
		ctx.moveTo(0, 0);
		ctx.lineTo(size, 0);
		ctx.lineTo(size, size);
		ctx.lineTo(0, size);
		ctx.closePath();
		ctx.clip();
		if (this.hover || this.down) {
			ctx.beginPath();
			ctx.moveTo(r + n, n);
			ctx.lineTo(size - r - n, n);
			ctx.arc(size - r - n, r + n, r, -d, 0, false);
			ctx.lineTo(size - n, size - r - n);
			ctx.arc(size - r - n, size - r - n, r, 0, d, false);
			ctx.lineTo(r + n, size - n);
			ctx.arc(r + n, size - r - n, r, d, 2 * d, false);
			ctx.lineTo(n, r + n);
			ctx.arc(r + n, r + n, r, 2 * d, 3 * d, false);
			ctx.fillStyle = this.down ? 'rgba(108, 200, 158, 0.5)' : 'rgba(108, 108, 158, 0.3)';
			ctx.fill();
		}
		ctx.save();
		ctx.shadowColor = 'rgba(0, 50, 200, 0.7)';
		var bs = size - 2 * border;
		var dy = (this.down) ? 0 : -2;
		if (dy) {
			ctx.shadowBlur = 5;
			ctx.shadowOffsetX = -2;
			ctx.shadowOffsetY = +2;
		} else {
			ctx.shadowBlur = 1;
		}
		ctx.drawImage(img, this.offset, 0, bs, bs, border, border + dy, bs, bs);
		ctx.restore();
		if (this.hover || this.down) {
			ctx.lineWidth = 2;
			ctx.strokeStyle = 'rgba(0, 0, 0, 1)';
			ctx.stroke();
		}
		ctx.restore();
	}
};


function ButtonGroup(manager, img, layout, mode, border) {
	this.manager = manager;
	this.img = img;
	this.vertical = layout;
	this.mode = mode;
	this.border = border ? border : 0;
	this.x = 0;
	this.y = 0;
	this.size = img.height + 2 * this.border;
	this.w = (this.vertical) ? this.size : 0;
	this.h = (this.vertical) ? 0 : this.size;
	this.buttons = [];
}
ButtonGroup.HORIZONTAL = false;
ButtonGroup.VERTICAL = true;
ButtonGroup.MODE_HIDDEN = 0;
ButtonGroup.MODE_NORMAL = 1;
ButtonGroup.MODE_RADIO = 2;
ButtonGroup.prototype.addButton = function(idx, action) {
	this.buttons.push(new Button(this, (this.size - 2 * this.border) * idx, (this.vertical) ? this.h : this.w, action));
	this.vertical ? this.h += this.size : this.w += this.size;
};
ButtonGroup.prototype.addSpace = function(size) {
	this.buttons.push({
		delta: (this.vertical) ? this.h : this.w,
		size: size
	});
	this.vertical ? this.h += size : this.w += size;
};
ButtonGroup.prototype.draw = function() {
	for (var i = 0; i < this.buttons.length; i++) {
		if (this.buttons[i].draw) this.buttons[i].draw();
	}
};
ButtonGroup.prototype.mouseMove = function(mx, my) {
	var delta = (this.vertical) ? my : mx;
	for (var i = 0; i < this.buttons.length; i++) {
		if (delta < this.buttons[i].delta + (this.buttons[i].size ? this.buttons[i].size : this.size)) {
			return (this.buttons[i].draw) ? this.buttons[i] : null;
		}
	}
	return null;
};


/**
 * Manages buttons and their mouse events.
 * @param {Visualizer} vis the visualizer
 */
function ButtonManager(vis) {
	this.vis = vis;
	this.groups = {};
	this.hover = null;
	this.nailed = null;
}
/**
 * @returns {ButtonGroup} the created button group
 */
ButtonManager.prototype.addGroup = function(name, img, layout, mode, border) {
	return this.groups[name] = new ButtonGroup(this, img, layout, mode, border);
};
ButtonManager.prototype.draw = function() {
	for (var name in this.groups) with (this.groups[name]) {
		if (mode != ButtonGroup.MODE_HIDDEN) {
			draw();
		}
	}
};
ButtonManager.prototype.mouseMove = function(mx, my) {
	var result = null;
	for (var name in this.groups) with (this.groups[name]) {
		if (mode != ButtonGroup.MODE_HIDDEN && my >= y && my < y + h && mx >= x && mx < x + w) {
			mx -= x;
			my -= y;
			result = mouseMove(mx, my);
			if (result !== null) {
				break;
			}
		}
	}
	if (this.hover !== result) {
		if (this.hover) {
			if (this.hover.hover || this.hover.down) {
				this.hover.hover = false;
				this.hover.down &= this.hover.group.mode == ButtonGroup.MODE_RADIO;
				this.hover.draw();
			}
		}
		if (result && (!this.nailed || this.nailed === result)) {
			if (!result.hover) {
				result.hover = true;
				result.down = (result === this.nailed) || (result.down && this.hover.group.mode == ButtonGroup.MODE_RADIO);
				result.draw();
			}
		}
		this.hover = result;
		this.repaintCheck();
	}
	return result;
};
ButtonManager.prototype.mouseUp = function() {
	if (this.nailed) {
		this.nailed.mouseUp();
		if (this.nailed == this.hover) {
			this.nailed.action();
		}
		this.nailed = null;
		this.repaintCheck();
	}
};
ButtonManager.prototype.mouseDown = function() {
	if (this.hover) {
		this.hover.mouseDown();
		this.nailed = this.hover;
		this.repaintCheck();
	}
};
/**
 * @private
 */
ButtonManager.prototype.repaintCheck = function() {
	if (this.vis.options.java) {
		this.vis.main.element.repaint();
	}
};


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

function AppletManager() {
	/**
	 * @private
	 */
	this.callbacks = {};
	/**
	 * @private
	 */
	this.tokenId = 0;
}
AppletManager.prototype.add = function(user) {
	this.callbacks[this.tokenId] = user;
	return this.tokenId++;
};
AppletManager.prototype.appletInitialized = function(token) {
	var vis = this.callbacks[token];
	if (vis) {
		delete this.callbacks[token];
		vis.initializedApplet(token);
	} else {
		alert('no applet with token: ' + token + ' in list');
	}
};
/**
 * Opera has a lose understanding of JavaScript's single-threadedness. A
 * call from an applet halts the currently executing code, executes the 
 * code called by the applet, then resumes with the previous operation.
 * Since this results in race conditions I defer the applet's call until
 * any active operation is completed. The next issue is Safari, which
 * has trouble decoding and Object[] which contains
 * The arguments will be passed using
 * func.apply(thisArg, argArray)
 * @param {Object} thisArg the object the function schould be called on;
 *        will be 'this' in the function
 * @param {String} func the function to be called
 * @param varArgs the applet will append any number of arguments here;
 *     they will be retrieved through the arguments variable
 */
AppletManager.prototype.callAfterExecutionUnit = function(thisArg, func, varArgs) {
	var jsArray = new Array(arguments.length - 2);
	for (var i = 0; i < jsArray.length; i++) {
		jsArray[i] = arguments[i + 2];
	}
	window.setTimeout(function() {
		thisArg[func].apply(thisArg, jsArray);
	}, 0);
};

appletManager = new AppletManager();
