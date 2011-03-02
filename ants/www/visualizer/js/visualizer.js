/**
 * @fileoverview This is a visualizer for the ai challenge ant game.
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

var cnt = 0;

/**
 * This global contains constants used throughout the program. I collected them
 * here because NetBeans otherwise clutters the symbol viewer.
 */
Const = {
	LEFT_PANEL_W: 0,         // width of left side panel
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


Buttons = {
	GOTO_START: 0,
	STEP_BW: 1,
	BACKWARD: 2,
	PLAY_PAUSE: 3,
	FORWARD: 4,
	STEP_FW: 5,
	GOTO_END: 6
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


function ImageManager(baseDirectory) {
	this.base = baseDirectory;
	this.images = [];
	this.patterns = [];
	this.error = '';
	this.pending = 0;
}
ImageManager.prototype.request = function(source) {
	var image = new Image();
	var that = this;
	image.onerror = function() {
        if (that.error) {
            that.error += '\n';
        }
        that.error += this.src + ' did not load.';
	};
	image.onabort = image.onerror;
	image.onload = function() {
        that.pending--;
	};
	this.pending++;
	this.images.push(image);
	this.patterns.push(null);
	image.src = this.base + source;
	return this.images.length - 1;
};
ImageManager.prototype.pattern = function(idx, ctx, repeat) {
	if (!this.patterns[idx]) {
		this.patterns[idx] = ctx.createPattern(this.images[idx], repeat);
	}
	ctx.fillStyle = this.patterns[idx];
};
ImageManager.prototype.complete = function(id) {
	if (id) {
		return this.images[id].complete;
	} else if (this.error !== '') {
		throw this.error;
	}
	return this.pending === 0;
};


/**
 * The main 'application' object that provides all necessary methods for the use
 * in a web page.
 * @param {Node} container the html element, that the visualizer will embed into
 * @param {string} dataDir This relative path to the visualizer data files. You
 *     will get an error message if you forget the tailing '/'.
 */
function Visualizer(container, dataDir) {
	/**
	 * any generated DOM elements will be placed here
	 * @private
	 */
	this.container = container;
	/**
	 * This field will be set to true by createCanvas() if we are running IE < 9
	 * and cannot use all canvas features.
	 */
	this.vml = false;
	/**
	 * the main canvas
	 * @private
	 */
	this.canvas = this.createCanvas();
	/**
	 * ...and it's 2D context
	 * @private
	 */
	this.ctx2d = this.canvas.getContext('2d');
	/**
	 * contains the backdrop for the map
	 * @private
	 */
	this.cvsMap = undefined;
	/**
	 * ...associated 2D context
	 * @private
	 */
	this.ctxMap = undefined;
	/**
	 * Caches the graphics of the map border
	 * @private
	 */
	this.cvsBorder = undefined;
	/**
	 * ...associated 2D context
	 * @private
	 */
	this.ctxBorder = undefined;
	/**
	 * array of precomputed turn data
	 * @private
	 */
	this.turns = undefined;
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
	 * images used by the visualizer
	 * @private
	 */
	this.imgMgr = new ImageManager((dataDir || '') + 'img/');
	this.imgMgr.request('wood.jpg');
	this.imgMgr.request('playback.png');
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
	/**
	 * buttons
	 * @private
	 */
	this.btnMgr = new ButtonManager(this);
	/**
	 * @private
	 */
	this.fullRedraw = false;
}
/**
 * Creates a canvas element and binds Google's VML wrapper to it
 * if the browser is IE less than 9.
 * @private
 */
Visualizer.prototype.createCanvas = function() {
    var result = document.createElement('canvas');
    if (!result.getContext) {
		this.vml = true;
        G_vmlCanvasManager.initElement(result);
    }
    return result;
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
	var unquot = data.replace(/&lt;/g, '<').replace(/&gt;/g, '>')
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
	} else if (data) {
		this.errorOut(data);
	} else {
		this.errorOut('no data');
	}
};
/**
 * Loads a replay string directly.
 * @param {string} data the replay string
 */
Visualizer.prototype.loadReplayData = function(data) {
	this.replay = data;
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
	while (this.container.childNodes.length > 0 ) {
		this.container.removeChild(this.container.firstChild);
	}
	this.container.appendChild(errorParagraph);
};
/**
 * Places the visualizer in a given container.
 * @param {number} width if defined, the maximum width in pixels
 * @param {number} height if defined, the maximum height in pixels
 */
Visualizer.prototype.attach = function(width, height) {
	var text;
	this.w = width;
	this.h = height;
	// replace the replay string, by it's parsed counter part
	if (this.replay) {
		try {
			this.replay = new Replay(this.replay);
		} catch (error) {
			text = 'Replay cannot be parsed!\n\n' + (error.stack ? 'Stack trace:\n' + error.stack : error);
			this.errorOut(text);
			this.replay = undefined;
		}
	}
	if (this.replay) {
		this.initWaitForImages();
	}
};
/**
 * @private
 */
Visualizer.prototype.initWaitForImages = function() {
	try {
		if (!this.imgMgr.complete()) {
			window.setTimeout('visualizer.initWaitForImages()', 50);
			return;
		}
	} catch (error) {
		this.errorOut(error);
		return;
	}
	if (this.canvas.parentNode !== this.container) {
		this.container.appendChild(this.canvas);
	}
	// this will fire once in FireFox when a key is held down
	var that = this;
	document.onkeydown = function(event) {
		if (!event) {
			// IE doesn't pass this as an argument
			event = window.event;
		}
		with (that.director) {
			switch(event.keyCode) {
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
					switch (String.fromCharCode(event.keyCode)) {
						case 'F':
							visualizer.setFullscreen(!visualizer.config.fullscreen);
							break;
					}
			}
		}
	};
	document.onkeyup = function() {
	};
	// this will fire repeatedly in all browsers
	document.onkeypress = function() {
	};
	this.canvas.onmousemove = function(event) {
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
		that.btnMgr.mouseMove(mx, my);
	};
	this.canvas.onmouseout = function() {
		that.btnMgr.mouseMove(-1, -1);
	};
	this.canvas.onmousedown = function() {
		that.btnMgr.mouseDown();
	};
	this.canvas.onmouseup = function() {
		that.btnMgr.mouseUp();
	};
	window.onresize = function() {
		that.resize();
	};
	if (!this.vml) {
		this.cvsMap = this.createCanvas();
		this.ctxMap = this.cvsMap.getContext('2d');
		this.cvsBorder = this.createCanvas();
	}
	if (!this.btnMgr.groups.playback) {
		with (this.btnMgr.addGroup('playback', this.imgMgr.images[1], ButtonGroup.HORIZONTAL, true, 2)) {
			addButton(3, function() {that.director.gotoTick(0)});
			addSpace(32);
			addButton(5, function() {that.director.gotoTick(Math.ceil(that.director.position) - 1)});
			//drawImage(this.imgMgr.images[1], 0 * 64, 0, 64, 64, x + 2.5 * 64, y, 64, 64);
			addSpace(64);
			addButton(4, function() {that.director.playStop()});
			//drawImage(this.imgMgr.images[1], 1 * 64, 0, 64, 64, x + 4.5 * 64, y, 64, 64);
			addSpace(64);
			addButton(6, function() {that.director.gotoTick(Math.floor(that.director.position) + 1)});
			addSpace(32);
			addButton(2, function() {that.director.gotoTick(that.director.duration)});
		}
	}
	Visualizer.prototype.focused = this;
	this.setFullscreen(this.config.fullscreen);
	// try to make the replays play 1 minute, but the turns take no more than a second
	this.director.duration = this.replay.turns.length - 2;
	this.director.defaultSpeed = Math.max(this.director.duration / 60, 1);
	this.director.onstate = function() {
		var btn = that.btnMgr.groups.playback.buttons[4];
		btn.offset += (that.director.playing() ? 3 : -3) * that.imgMgr.images[1].height;
		if (btn === that.btnMgr.nailed) {
			that.btnMgr.nailed = null;
		}
		btn.down = false;
		btn.draw();
	};
	this.director.play();
};
Visualizer.prototype.resize = function(forced) {
	var iw, ih;
	if (typeof(window.innerWidth) == 'number' ) {
		//Non-IE
		iw = window.innerWidth;
		ih = window.innerHeight;
	} else if (document.documentElement && (document.documentElement.clientWidth || document.documentElement.clientHeight)) {
		//IE 6+ in 'standards compliant mode'
		iw = document.documentElement.clientWidth;
		ih = document.documentElement.clientHeight;
	} else if (document.body && (document.body.clientWidth || document.body.clientHeight)) {
		//IE 4 compatible
		iw = document.body.clientWidth;
		ih = document.body.clientHeight;
	}
  	iw = (this.w && !this.config.fullscreen ? this.w : iw);
	ih = (this.h && !this.config.fullscreen ? this.h : ih - 4);
	if (iw != this.canvas.width || ih != this.canvas.height || forced) {
		this.canvas.width = iw;
		this.canvas.height = ih;
		this.loc.vis = {
			w: iw - Const.LEFT_PANEL_W,
			h: ih - Const.TOP_PANEL_H - Const.BOTTOM_PANEL_H,
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
		if (!this.vml) {
			this.cvsBorder.width = this.loc.map.w + 2 * Const.ZOOM_SCALE;
			this.cvsBorder.height = this.loc.map.h + 2 * Const.ZOOM_SCALE;
			this.ctxBorder = undefined;
			this.renderBorder();
			this.cvsMap.width = this.loc.map.w;
			this.cvsMap.height = this.loc.map.h;
			this.renderMap(this.ctxMap, 0, 0);
		}
		with (this.btnMgr.groups) {
			playback.x = ((iw - 8 * 64) / 2) | 0;
			playback.y = this.loc.vis.y + this.loc.vis.h;
		}
		this.redrawEverything();
	}
};
/**
 * @private
 */
Visualizer.prototype.redrawEverything = function() {
	var ctx = this.drawContext();
	if (!ctx) return;
	this.btnMgr.draw();
	with (ctx) {
		// draw player names and captions
		if (!this.vml) {
			var colors = Const.PLAYER_COLORS;
			textAlign = 'left';
			textBaseline = 'top';
			font = 'bold 20px sans-serif';
			var x = 0;
			for (i = 0; i < this.replay.players.length; i++) {
				fillStyle = 'rgb(' + colors[i][0] + ', ' + colors[i][1] + ', ' + colors[i][2] + ')';
				fillText(this.replay.players[i].name, x, 2);
				x += this.ctx2d.measureText(this.replay.players[i].name).width;
				if (i != this.replay.players.length - 1) {
					fillStyle = '#888';
					fillText(' vs ', x, 2);
					x += measureText(' vs ').width;
				}
			}
			var w = this.canvas.width;
			fillStyle = '#000';
			textAlign = 'center';
			textBaseline = 'middle';
			font = 'bold 12px sans-serif';
			fillText('ants'  , 30          , Const.TOP_PANEL_H - 10);
			fillText('scores', 30 + 0.5 * w, Const.TOP_PANEL_H - 10);
		}
	}
	this.director.draw();
}
/**
 * @private
 */
Visualizer.prototype.renderMap = function(ctx2d, x, y) {
	ctx2d.fillStyle = Const.COLOR_SAND;
	ctx2d.fillRect(x, y, this.loc.map.w, this.loc.map.h);
	ctx2d.fillStyle = Const.COLOR_WATER;
	for (var row = 0; row < this.replay.parameters.rows; row++) {
		var start = undefined;
		for (var col = 0; col < this.replay.parameters.cols; col++) {
			var isWall = this.replay.walls[row][col];
			if (start === undefined && isWall) {
				start = col;
			} else if (start !== undefined && !isWall) {
				ctx2d.fillRect(this.scale * start + x, this.scale * row + y, this.scale * (col - start), this.scale);
				start = undefined;
			}
		}
		if (start !== undefined) {
			ctx2d.fillRect(this.scale * start + x, this.scale * row + y, this.scale * (col - start), this.scale);
		}
	}
}
/**
 * @private
 */
Visualizer.prototype.renderBorder = function() {
	if (!this.ctxBorder) {
		if (!this.vml) {
			this.ctxBorder = this.cvsBorder.getContext('2d');
		}
		with (this.vml ? this.ctx2d : this.ctxBorder) with(Const) {
			save();
				if (this.vml) {
					this.ctx2d.fillStyle = '#edb';
					translate(this.loc.map.x, this.loc.map.y);
				} else {
					this.imgMgr.pattern(0, this.ctxBorder, 'repeat');
					translate(ZOOM_SCALE, ZOOM_SCALE);
				}
				with (this.loc.map) {
					save();
						beginPath();
						moveTo(0, 0);
						lineTo(w, 0);
						lineTo(w + ZOOM_SCALE, -ZOOM_SCALE);
						lineTo(-ZOOM_SCALE, -ZOOM_SCALE);
						closePath();
						clip();
						fillRect(-ZOOM_SCALE, -ZOOM_SCALE, w + 2 * ZOOM_SCALE, ZOOM_SCALE);
					restore();
					save();
						translate(0, h);
						beginPath();
						moveTo(0, 0);
						lineTo(w, 0);
						lineTo(w + ZOOM_SCALE, +ZOOM_SCALE);
						lineTo(-ZOOM_SCALE, +ZOOM_SCALE);
						closePath();
						clip();
						fillRect(-ZOOM_SCALE, 0, w + 2 * ZOOM_SCALE, ZOOM_SCALE);
					restore();
					rotate(0.5 * Math.PI);
					save();
						beginPath();
						moveTo(0, 0);
						lineTo(h, 0);
						lineTo(h + ZOOM_SCALE, ZOOM_SCALE);
						lineTo(-ZOOM_SCALE, ZOOM_SCALE);
						closePath();
						clip();
						fillRect(-ZOOM_SCALE, + ZOOM_SCALE, h + 2 * ZOOM_SCALE, -ZOOM_SCALE);
					restore();
					save();
						translate(0, -w);
						beginPath();
						moveTo(0, 0);
						lineTo(h, 0);
						lineTo(h + ZOOM_SCALE, -ZOOM_SCALE);
						lineTo(-ZOOM_SCALE, -ZOOM_SCALE);
						closePath();
						clip();
						fillRect(-ZOOM_SCALE, -ZOOM_SCALE, h + 2 * ZOOM_SCALE, ZOOM_SCALE);
				   restore();
				}
			restore();
		}
	}
	if (!this.vml) {
		this.ctx2d.drawImage(this.cvsBorder, this.loc.map.x - Const.ZOOM_SCALE, this.loc.map.y - Const.ZOOM_SCALE);
	}
}
/**
 * @private
 */
Visualizer.prototype.drawColorBar = function(x, y, w, h, values, colors) {
	var sum = 0;
	with (this.ctx2d) {
		save();
		beginPath();
		rect(x, y, w, h);
		clip();
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
			fillStyle = 'rgb(' + colors[i][0] + ', ' + colors[i][1] + ', ' + colors[i][2] + ')';
			fillRect(offsetX, y, w - offsetX + x, h);
			offsetX += amount;
		}
		if (!this.vml) {
			textAlign = 'left';
			textBaseline = 'middle';
			font = 'bold 16px Monospace';
			fillStyle = 'rgba(0,0,0,0.5)';
			var offsetY = y + 0.5 * h;
			offsetX = x + 2;
			for (i = 0; i < useValues.length; i++) {
				if (useValues[i] != 0) {
					fillText(Math.round(values[i]), offsetX, offsetY);
				}
				offsetX += scale * useValues[i];
			}
		}
		restore();
	}
};
/**
 * @private
 */
Visualizer.prototype.correctCoords = function(obj, w, h) {
	obj.x -= Math.floor(obj.x / w) * w;
	obj.y -= Math.floor(obj.y / h) * h;
}
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
Visualizer.prototype.draw = function(time, tick) {
	// vml gets special treatment:
	if (!this.drawContext()) return;
	cnt++;
	var cx, cy, mx, my;
	var drawOrder = [];
	var turn = (time | 0) + 1;
	// draw the map background
	if (this.vml) {
		this.renderMap(this.ctx2d, this.loc.map.x, this.loc.map.y);
	} else {
		this.ctx2d.drawImage(this.cvsMap, this.loc.map.x, this.loc.map.y);
	}
	var w = this.canvas.width;
	var h = this.loc.vis.h - 20 - Const.TOP_PANEL_H;
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
			this.ctx2d.save();
			this.ctx2d.globalAlpha = antObj.alpha;
			cx = Const.ZOOM_SCALE * (antObj.x + 0.5);
			cy = Const.ZOOM_SCALE * (antObj.y + 0.5);
			this.ctx2d.translate(cx, cy);
			this.ctx2d.rotate(antObj.angle + Math.sin(20 * time) * antObj.jitter);
			this.ctx2d.drawImage(this.imgMgr.ants[antObj.type], -10, -10);
			this.ctx2d.restore();
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
			this.ctx2d.fillStyle = 'rgba(' + antObj.r + ', ' + antObj.g + ', ' + antObj.b + ', ' + antObj.a + ')';
			this.ctx2d.fillRect(mx, my, this.scale, this.scale);
		}
	}
	// draw border over ants, that moved out of the map
	this.renderBorder();
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
	this.resize(true);
}

Visualizer.prototype.drawContext = function() {
	if (this.fullRedraw || !this.vml) {
		return this.ctx2d;
	} else {
		var timeA = new Date().getTime();
		cnt = 0;
		this.fullRedraw = true;
		this.ctx2d.clearRect();
		this.redrawEverything();
		this.fullRedraw = false;
		var timeB = new Date().getTime();
		document.title = 'Render time for ' + cnt + ' items: ' + (timeB - timeA) + ' ms';
		return null;
	}
}


/**
 * The director is supposed to keep track of playback speed and position.
 */
function Director(client) {
	this.speed = 0;
	this.position = 0;
	this.lastTime = undefined;
	this.client = client;
	this.duration = 0;
	this.defaultSpeed = 1;
	this.cpu = 0.5;
	this.onstate = undefined;
}
/**
 * When the director is in playback mode it has a lastTime. This is just a
 * convenience method of querying the playback mode.
 * @type boolean
 */
Director.prototype.playing = function() {
	return this.lastTime !== undefined;
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
		this.loop(false);
		if (this.onstate) this.onstate();
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
		this.client.draw(this.position, oldTick !== tick);
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
	} else if (this.position >= this.duration && this.speed > 0) {
		this.position = this.duration;
		goOn = false;
	}
	this.client.draw(this.position, (oldTurn != (this.position | 0)) ? this.position | 0 : undefined);
	if (goOn) {
		var that = this;
		var cpuTime = new Date().getTime() - this.lastTime;
		var delay = (this.cpu <= 0 || this.cpu > 1)
			? 0 : Math.ceil(cpuTime / this.cpu - cpuTime);
		window.setTimeout(function() {that.loop(true)}, delay);
	} else {
		this.stop();
	}
};
/**
 * Causes the visualizer to draw the current game state. For performance reasons
 * this is not done when the visualizer is already playing back anyway.
 */
Director.prototype.draw = function() {
	if (!this.playing() || this.client.vml) {
		this.client.draw(this.position, this.position | 0);
	}
};


function Button(group, offset, delta, action) {
	this.group = group;
	this.offset = offset;
	this.delta = delta;
	this.action = action;
	this.hover = false;
	this.down = false;
}
Button.prototype.draw = function() {
	var ctx = this.group.manager.controller.drawContext();
	if (!ctx) return;
	var vml = this.group.manager.controller.vml;
	with (this.group) with(ctx) {
		var ix = x + (vertical ? 0 : this.delta);
		var iy = y + (vertical ? this.delta : 0);
		var n = 1;
		var r = 10;
		var d = 0.5 * Math.PI;
		save();
		translate(ix, iy);
		// any clearRect call would erase the screen
		if (!vml) clearRect(0, 0, size, size);
		beginPath();
		moveTo(0, 0);
		lineTo(size, 0);
		lineTo(size, size);
		lineTo(0, size);
		closePath();
		clip();
		if (this.hover) {
			beginPath();
			moveTo(r + n, n);
			lineTo(size - r - n, n);
			arc(size - r - n, r + n, r, -d, 0, false);
			lineTo(size - n, size - r - n);
			arc(size - r - n, size - r - n, r, 0, d, false);
			lineTo(r + n, size - n);
			arc(r + n, size - r - n, r, d, 2 * d, false);
			lineTo(n, r + n);
			arc(r + n, r + n, r, 2 * d, 3 * d, false);
			fillStyle = this.down ? 'rgba(108, 200, 158, 0.5)' : 'rgba(108, 108, 158, 0.3)';
			fill();
			lineWidth = 2;
			strokeStyle = 'rgba(0, 0, 0, 1)';
			stroke();
		}
		shadowColor = 'rgba(0, 50, 200, 0.7)';
		var bs = size - 2 * border;
		var dy = (this.hover && this.down) ? 0 : -2;
		if (dy) {
			shadowBlur = 5;
			shadowOffsetX = -2;
			shadowOffsetY = +2;
		} else {
			shadowBlur = 1;
		}
		drawImage(img, this.offset, 0, bs, bs, border, border + dy, bs, bs);
		restore();
	}
};


function ButtonGroup(manager, img, layout, visible, border) {
	this.manager = manager;
	this.img = img;
	this.vertical = layout;
	this.visible = visible;
	this.border = border ? border : 0;
	this.x = 0;
	this.y = 0;
	this.size = (this.vertical ? img.width : img.height) + 2 * this.border;
	this.w = (this.vertical) ? this.size : 0;
	this.h = (this.vertical) ? 0 : this.size;
	this.buttons = [];
}
ButtonGroup.prototype.HORIZONTAL = false;
ButtonGroup.prototype.VERTICAL = true;
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
 * @param controller the visualizer providing a redraw() method to accomodate
 *     for the vml version (ExplorerCanvas)
 */
function ButtonManager(controller) {
	this.controller = controller;
	this.groups = {};
	this.hover = null;
	this.nailed = null;
}
ButtonManager.prototype.addGroup = function(name, img, layout, visible, border) {
	return this.groups[name] = new ButtonGroup(this, img, layout, visible, border);
};
ButtonManager.prototype.draw = function() {
	for (var name in this.groups) with (this.groups[name]) {
		if (visible) {
			draw();
		}
	}
};
ButtonManager.prototype.mouseMove = function(mx, my) {
	var result = null;
	for (var name in this.groups) with (this.groups[name]) {
		if (my >= y && my < y + h && mx >= x && mx < x + w) {
			mx -= x;
			my -= y;
			result = mouseMove(mx, my);
			if (result !== null) {
				break;
			}
		}
	}
	if ((this.hover ? result === null : result !== null) && this.hover !== result) {
		if (this.hover) {
			if (this.hover.hover) {
				this.hover.hover = false;
				this.hover.draw();
			}
		}
		if (result) {
			if (!result.hover) {
				result.hover = true;
				result.draw();
			}
		}
		this.hover = result;
	}
	return result;
};
ButtonManager.prototype.mouseUp = function() {
	if (this.nailed) {
		this.nailed.down = false;
		this.nailed.draw();
		if (this.nailed == this.hover) {
			this.nailed.action();
		}
		this.nailed = null;
	}
};
ButtonManager.prototype.mouseDown = function() {
	if (this.hover) {
		this.hover.down = true;
		this.hover.draw();
		this.nailed = this.hover;
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
