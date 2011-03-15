$import('Ant');
$import('Const');
$import('LongText');

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

/**
 * Constructs a new direction from the given coordinates. X points to the rigtht
 * and Y points to the bottom. Up is 0°, Right is 90° and so on.
 * @class A compass direction
 */
function Direction(x, y) {
	this.x = x;
	this.y = y;
	this.angle = Math.atan2(x, -y);
}

// translate compass directions to movement offsets
Directions = {
	N : new Direction( 0, -1),
	NE: new Direction(+1, -1),
	E : new Direction(+1,  0),
	SE: new Direction(+1, +1),
	S : new Direction( 0, +1),
	SW: new Direction(-1, +1),
	W : new Direction(-1,  0),
	NW: new Direction(-1, -1)
};

DataType = {
	STRING: function(p) {
		return [ p, null ];
	},
	IDENT: function(p) {
		p = p.match(DataType.MATCH);
		return [ p[1], p[2] ];
	},
	UINT: function(p, n) {
		p = p.match(DataType.MATCH);
		p = [ parseInt(p[1]), p[2] ];
		if (isNaN(p[0]) || p[0] < 0) {
			throw new Error('Parameter ' + n + ' must be an unsigned integer.');
		}
		return p;
	},
	POSINT: function(p, n) {
		p = DataType.UINT(p, n);
		if (p[0] <= 0) {
			throw new Error('Parameter ' + n + ' must be a positive integer.');
		}
		return p;
	},
	NUMBER: function(p, n) {
		p = p.match(DataType.MATCH);
		p = [ parseFloat(p[1]), p[2] ];
		if (isNaN(p[0])) {
			throw new Error('Parameter ' + n + ' is not a number.');
		}
		return p;
	},
	COLOR: function(p, n) {
		p = p.match(DataType.MATCH);
		p[0] = new Array(3);
		if (p[1].charAt(0) != '#') {
			throw new Error('Parameter ' + n + ' is not a html color code.');
		}
		var chars = (p[1].length / 3) | 0;
		for (var i = 0; i < 3; i++) {
			p[0][i] = parseInt(p[1].substr(1 + chars * i, chars), 16);
		}
		return [ p[0], p[2] ];
	},
	MAP: function(p, n) {
		p = p.match(DataType.MATCH);
		if (!p[1]) {
			throw new Error('Parameter ' + n + ' must not be empty.');
		}
		p[0] = new Array(p[1].length);
		for (var col = 0; col < p[1].length; col++) {
			var c = p[1].charAt(col);
			if (c !== '%' && c !== '*' && c !== '.' && (c < 'a' || c > 'z')) {
				throw new Error('Invalid character in map line: ' + c);
			}
			p[0][col] = (c === '%');
		}
		return [ p[0], p[2] ];
	},
	ORDERS: function(p) {
		p = p.match(DataType.MATCH);
		p[1] = p[1].toUpperCase().split('');
		p[0] = new Array(p[1].length);
		for (var turn = 0; turn < p[1].length; turn++) {
			var c = p[1][turn];
			if (c !== 'N' && c !== 'E' && c !== 'S' && c !== 'W' && c !== '-') {
				throw new Error('Invalid character in orders line: ' + c);
			}
			p[0][turn] = Directions[c];
		}
		return [ p[0], p[2] ];
	},
	SCORES: function(p) {
		p = p.replace(/\s+/g, ' ').replace(/\s*$/, '').split(' ');
		for (var i = 0; i < p.length; i++) {
			p[i] = parseFloat(p[i]);
			if (isNaN(p[i])) {
				throw new Error('Score ' + i + ' is not a number.');
			}
		}
		return [ p, null ];
	},
	MATCH: /(\S*)\s*(.*)/
};

function Replay(replayStr, parameters) {
	var tl;
	var lit = new LineIterator(replayStr);
	this.turns = [];
	var ants = {};
	var autoinc = 0;
	try {
		// version check
		tl = lit.gimmeNext().kw('v').as([DataType.IDENT, DataType.POSINT]);
		tl.expectEq(0, 'ants'); // game name
		tl.expectEq(1, 1);      // file version
		// players
		tl = lit.gimmeNext().kw('players').as([DataType.POSINT]);
		tl.expectLE(0, 26);     // player count <= 26
		this.players = new Array(tl.params[0]);
		for (var i = 0; i < this.players.length; i++) {
			this.players[i] = {name: 'player ' + (i + 1)};
		}
		// parameters
		this.parameters = parameters || {};
		tl = lit.gimmeNext();
		while (tl.keyword !== 'm') {
			if (tl.keyword.substr(0, 6) === 'player') {
				var slot = tl.keyword.substr(6);
				var args = [DataType.UINT, DataType.STRING];
				if (slot === 'color') args[1] = DataType.COLOR;
				tl.as(args);
				var player = this.getPlayer(tl.params[0]);
				player[slot] = tl.params[1];
			} else {
				args = [DataType.STRING];
				if (tl.keyword === 'viewradius2') {
					args[0] = DataType.UINT;
				}
				tl.as(args);
				this.parameters[tl.keyword] = tl.params[0];
			}
			tl = lit.gimmeNext();
		}
		// map
		this.walls = [];
		var cols = undefined;
		var rows = 0;
		do {
			tl.as([DataType.MAP]);
			if (cols === undefined) {
				cols = tl.params[0].length;
			} else if (tl.params[0].length !== cols) {
				throw new Error('Map lines have different lenghts');
			}
			this.walls.push(tl.params[0]);
			rows++;
			tl = lit.gimmeNext();
		} while (tl.keyword === 'm');
		this.rows = this.walls.length;
		this.cols = this.walls[0].length;
		// food / ant
		while (tl.keyword === 'f' || tl.keyword === 'a') {
			var isAnt = (tl.keyword === 'a');
			var owner = undefined;
			if (isAnt) {
				tl.as([DataType.UINT, DataType.UINT, DataType.UINT, DataType.UINT, DataType.ORDERS]);
				owner = tl.params[0];
				if (owner >= this.players.length) throw new Error('Player index out of range.');
			} else {
				tl.as([DataType.UINT, DataType.UINT, DataType.UINT, DataType.UINT], 1);
			}
			var row = tl.params[isAnt ? 1 : 0];
			var col = tl.params[isAnt ? 2 : 1];
			var start = tl.params[isAnt ? 3 : 2];
			if (isAnt) {
				var orders = tl.params[4];
				var end = start + orders.length;
				var item = 'Ant';
			} else {
				end = tl.params[3];
				item = 'Food';
			}
			if (row >= this.rows || col >= this.cols) {
				throw new Error(item + ' spawned outside of map at row ' + row + ' / col ' + col);
			}
			while (this.turns.length < end + 1) {
				this.turns.push(new Turn(this.players.length, this.rows, this.cols));
			}
			// for a conversion, try to find previous food item
			var ant = null;
			if (isAnt) {
				for (var id in ants) {
					if (ants[id].ant.lo[0].x === col && ants[id].ant.lo[0].y === row && ants[id].end === start) {
						ant = ants[id].ant;
						ants[id].end = end;
						ants[id].tl = tl;
						break;
					}
				}
			}
			if (ant === null) {
				ant = new Ant(autoinc++, start - 1);
				var f = ant.frameAt(start - 1, Quality.LOW, false);
				f.x = col;
				f.y = row;
				f.r = Const.FOOD_COLOR[0];
				f.g = Const.FOOD_COLOR[1];
				f.b = Const.FOOD_COLOR[2];
				f.a = 0.0;
				ant.owner = undefined;
				ants[ant.id] = new AntLife(ant, start - 1, end, tl);
			}
			if (end && !isAnt && start === end) {
				throw new Error('Food has no lifespan.');
			}
			// perform conversion only in the last 25% of turn time
			if (isAnt && start > 0) {
				ant.frameAt(start - 1, Quality.LOW);
				ant.frameAt(start - 0.25, Quality.LOW, true);
			}
			if (end) for (i = start; i < end; i++) {
				if (isAnt) this.turns[i].counts[owner]++;
			}
			// in case of conversion, fade in the color
			f = ant.frameAt(start, Quality.LOW, true);
			if (isAnt) {
				ant.owner = owner;
				ant.frameAt(start - 0.25, Quality.LOW, true);
				f = ant.frameAt(start, Quality.LOW, true);
				f.r = Const.PLAYER_COLORS[owner][0];
				f.g = Const.PLAYER_COLORS[owner][1];
				f.b = Const.PLAYER_COLORS[owner][2];
			}
			// must have full opacity by the start of the turn
			f.a = 1.0;
			// do moves
			var x = col;
			var y = row;
			if (orders) for (i = 0; i < orders.length; i++) {
				var dir = orders[i];
				if (dir) {
					ant.frameAt(start + i, Quality.LOW, true);
					f = ant.frameAt(start + i + 0.5, Quality.LOW, true);
					f.x = Math.round(f.x + dir.x);
					f.y = Math.round(f.y + dir.y);
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
					x += dir.x;
					y += dir.y;
				}
				this.turns[start + i].clearFog(owner, y, x, this.rows, this.cols, this.parameters.viewradius2);
			}
			tl = lit.gimmeNext();
		}
		// score
		for (i = 0; i < this.players.length; i++) {
			var scores = tl.kw('s').as([DataType.SCORES]).params[0];
			while (this.turns.length < scores.length) {
				this.turns.push(new Turn(this.players.length));
			}
			for (var k = 0; k < scores.length; k++) {
				this.turns[k].scores[i] = scores[k];
			}
			for (; k < this.turns.length; k++) {
				this.turns[k].scores[i] = scores[scores.length - 1];
			}
			if (i != this.players.length - 1) tl = lit.gimmeNext();
		}
		for (id in ants) {
			item = ants[id];
			tl = item.tl;
			if (item.end === this.turns.length - 1 || item.end === undefined) {
				item.end = this.turns.length - 1;
				this.turns[this.turns.length - 1].counts[item.ant.owner]++;
			} else {
				if (item.start === item.end) {
					throw new Error('Found an ant with no lifespan. Birth and death in turn ' + (item.start + 1) + '.');
				} else {
					var collision = false;
					dir = collision ? item.end - 0.75 : item.end - 0.25;
					item.ant.fade(Quality.LOW, 'a', [
						{time: dir       , value: undefined},
						{time: dir + 0.25, value: 0.0},
						{time: item.end  , value: 0.0}
					]);
				}
			}
			for (i = Math.max(0, item.start); i <= item.end; i++) {
				this.turns[i].ants.push(item.ant);
			}
		}
		if (lit.moar()) {
			tl = lit.gimme();
			throw new Error('Extra data at end of file.');
		}
	} catch (error) {
		error.message = tl.line + '\n' + error.message;
		throw error;
	}
//	var nextAntDirection = function(id, turn) {
//		for (var nadk = 0; nadk < tturns[turn + 1].orders.length; nadk++) {
//			var nadAction = tturns[turn + 1].orders[nadk];
//			if (nadAction.id == id) {
//				return nadAction.direction;
//			}
//		}
//		return null;
//	}
}
Replay.prototype.getPlayer = function(n) {
	if (n >= this.players.length) {
		throw new Error('Player number ' + n + ' is out of range for per player parameter.');
	}
	return this.players[n];
};
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

/**
 * @class A highly optimized string tokenizer for replay files. It ignores blank
 *     lines and comment lines, trims and splits each line in two after the
 *     keyword. It processes a 220 KB file with over 27,000 lines in
 *     about 18 ms in Chromium on a 2,0 Ghz Core 2 Duo.
 */
function LineIterator(text) {
	// we keep a backup copy of the original for debugging purposes
	this.text = text;
	// eat comment lines and trim others; split text into lines
	this.lines = text.replace(TokenLine.NORMALIZE_REGEXP, '').split('\n');
	this.tokenLines = new Array(this.lines.length);
	// separate keyword from parameter list
	for (var i = 0; i < this.lines.length; i++) {
		this.tokenLines[i] = new TokenLine(this.lines[i]);
	}
	this.pos = 0;
}
LineIterator.prototype.gimmeNext = function() {
	if (this.pos < this.tokenLines.length) {
		return this.tokenLines[this.pos++];
	}
	throw new Error('Tried to read past the end of the file. Is it truncated?');
};
LineIterator.prototype.moar = function() {
	return this.pos < this.tokenLines.length;
};

function TokenLine(line) {
	this.line = line;
	var match = line.match(TokenLine.KEYWORD_REGEXP);
	this.keyword = match[1].toLowerCase();
	this.params = match[2];
}
TokenLine.NORMALIZE_REGEXP = /^([^\S\n]*(#.*)?\n)*|(\n[^\S\n]*(#.*)?)*$|\n[^\S\n]*(#.*)?(?=\n)/g;
TokenLine.KEYWORD_REGEXP = /(\S+)\s*(.*)/;
TokenLine.prototype.kw = function(keyword) {
	if (this.keyword !== keyword) {
		this.expected(keyword, this.keyword);
	}
	return this;
};
TokenLine.prototype.as = function(args, optional) {
	if (optional === undefined) optional = 0;
	var work = this.params;
	this.params = [];
	for (var i = 0; i < args.length; i++) {
		if (work || args.length - i > optional) {
			var parts = args[i](work);
			this.params.push(parts[0]);
			work = parts[1];
		}
	}
	if (work) throw new Error('The following unexpected additional parameter was found: ' + work);
	return this;
};
TokenLine.prototype.expected = function(expectation, reality) {
	throw new Error('Expected ' + expectation + ', but ' + reality + ' found.');
};
TokenLine.prototype.expectEq = function(idx, value) {
	if (value !== this.params[idx]) {
		this.expected(value, this.params[idx]);
	}
};
TokenLine.prototype.expectLE = function(idx, value) {
	if (value < this.params[idx]) {
		this.expected('parameter ' + idx + ' to be <= ' + value, this.params[idx]);
	}
};

function Turn(playerCnt, rows, cols) {
	this.scores = new Array(playerCnt);
	this.counts = new Array(playerCnt);
	this.fogs = new Array(playerCnt);
	for (var i = 0; i < playerCnt; i++) {
		this.counts[i] = 0;
		this.fogs[i] = new Array(rows);
		for (var k = 0; k < rows; k++) {
			this.fogs[i][k] = new Array(cols);
			for (var m = 0; m < cols; m++) {
				this.fogs[i][k][m] = true;
			}
		}
	}
	this.ants = [];
}
Turn.prototype.clearFog = function(player, row, col, rows, cols, radius2) {
	var fog = this.fogs[player];
	var radius = Math.ceil(Math.sqrt(radius2));
	for (var row_a = row - radius; row_a <= row + radius; row_a++) {
		var row_delta = row_a - row;
		var row_b = row_a - Math.floor(row_a / rows) * rows;
		for (var col_a = col - radius; col_a <= col + radius; col_a++) {
			var col_delta = col_a - col;
			if (row_delta * row_delta + col_delta * col_delta <= radius2) {
				var col_b = col_a - Math.floor(col_a / cols) * cols;
				fog[row_b][col_b] = false;
			}
		}
	}
};

function AntLife(ant, start, end, tl) {
	this.ant = ant;
	this.start = start;
	this.end = end;
	this.tl = tl;
}