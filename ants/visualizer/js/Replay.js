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
 * @constructor
 */
function Direction(x, y) {
	this['x'] = x;
	this['y'] = y;
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
		p[1] = p[1].split('');
		p[0] = new Array(p[1].length);
		for (var turn = 0; turn < p[1].length; turn++) {
			switch (p[1][turn]) {
				case 'n':
				case 'N':
					p[0][turn] = Directions.N;
					break;
				case 'e':
				case 'E':
					p[0][turn] = Directions.E;
					break;
				case 's':
				case 'S':
					p[0][turn] = Directions.S;
					break;
				case 'w':
				case 'W':
					p[0][turn] = Directions.W;
					break;
				case '-':
					p[0][turn] = null;
					break;
				default:
					throw new Error('Invalid character in orders line: ' + p[1][turn]);
			}
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

/**
 * @constructor
 */
function Replay(replayStr, parameters) {
	// check for meta data
	this.meta = {};
	if (replayStr.search(/^\s*{/) === 0) {
		this.meta = JSON.parse(replayStr);
		if (this.meta && (this.meta instanceof Object)) {
			var format = this.meta['replayformat'];
			if (this.meta['challenge'] !== 'ants') {
				throw new Error('This visualizer is for the ants challenge,'
						+ ' but a "' + this.meta['challenge']
						+ '" replay was loaded.');
			} else if (format !== 'storage') {
				throw new Error('Replays in the format "'
						+ this.meta['replayformat']
						+ '" are not supported.');
			}
			replayStr = this.meta['replaydata'];
			delete this.meta['replaydata'];
			if (!replayStr) {
				throw new Error('no data');
			}
		} else {
			throw new Error('replay meta data is no object notation');
		}
	}
	// start parsing process
	var tl, owner;
	var lit = new LineIterator(replayStr);
	this.turns = [];
	var autoinc = 0;
	var durationSetter = null;
	var that = this;
	var setReplayDuration = function(duration, fixed) {
		if (durationSetter) {
			if (!fixed && that.turns.length < duration + 1 || fixed && that.turns.length !== duration + 1) {
				throw new Error('Replay duration was previously set to ' + (that.turns.length - 1) + ' by the line "' + durationSetter.line + '" and is now redefined to be ' + duration);
			}
		} else {
			while (that.turns.length < duration + 1) {
				that.turns.push(new Turn(that.players, that.rows, that.cols));
			}
			if (fixed) durationSetter = tl;
		}
	};

	try {
		// version check
		tl = lit.gimmeNext();
		tl.kw('v').as([DataType.IDENT, DataType.POSINT]);
		tl.expectEq(0, 'ants'); // game name
		tl.expectEq(1, 1);      // file version
		// players
		tl = lit.gimmeNext();
		tl.kw('players').as([DataType.POSINT]);
		tl.expectLE(0, 26);     // player count <= 26
		this.players = tl.params[0];
		// add missing meta data
		if (!(this.meta['players'] instanceof Array)) {
			this.meta['players'] = new Array(this.players);
		}
		if (!(this.meta['playercolors'] instanceof Array)) {
			this.meta['playercolors'] = new Array(this.players);
		}
		for (i = 0; i < this.meta.players.length; i++) {
			if (!this.meta['players'][i]) {
				this.meta['players'][i] = 'player ' + (i + 1);
			}
			if (!(this.meta['playercolors'][i] instanceof Array)) {
				this.meta['playercolors'][i] = PLAYER_COLORS[i];
			}
		}
		// parameters
		this.parameters = parameters || {};
		tl = lit.gimmeNext();
		while (tl.keyword !== 'm') {
			var args = [DataType.STRING];
			if (tl.keyword === 'viewradius2') {
				args[0] = DataType.UINT;
			}
			tl.as(args);
			this.parameters[tl.keyword] = tl.params[0];
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
		while (tl.keyword === 'a') {
			//     row            col            start          conversion
			tl.as([DataType.UINT, DataType.UINT, DataType.UINT, DataType.UINT,
				DataType.UINT, DataType.UINT, DataType.ORDERS], 3);
			//  end            owner          orders            # optional
			var row = tl.params[0];
			if (row >= this.rows) throw new Error('Row exceeds map width.');
			var col = tl.params[1];
			if (col >= this.cols) throw new Error('Col exceeds map height.');
			var start = tl.params[2];
			var conv = tl.params[3];
			var end = tl.params[4];
			if (end === undefined) end = conv;
			owner = tl.params[5];
			var isAnt = owner !== undefined;
			if (isAnt && owner >= this.players) {
				throw new Error('Player index out of range.');
			}
			var orders = tl.params[6] || '';
			if (isAnt) {
				var fixed = orders.length !== end - conv;
				if (fixed && orders.length + 1 !== end - conv) {
					throw new Error('Number of orders does not match life span.');
				}
				setReplayDuration(end - 1, fixed);
			} else {
				setReplayDuration(conv - 1, false);
			}
			// create ant
			var ant = new Ant(autoinc++, start - 0.25);
			ant.owner = owner;
			var f = ant.frameAt(start - 0.25, Quality.LOW, false);
			f['x'] = col;
			f['y'] = row;
			f['r'] = FOOD_COLOR[0];
			f['g'] = FOOD_COLOR[1];
			f['b'] = FOOD_COLOR[2];
			if (start !== 0) {
				f = ant.frameAt(start, Quality.LOW, true);
				f['size'] = 1.0;
				f = ant.frameAt(start + 0.125, Quality.LOW, true);
				f['size'] = 1.5;
				f = ant.frameAt(start + 0.25, Quality.LOW, true);
				f['size'] = 0.7;
				f = ant.frameAt(start + 0.5, Quality.LOW, true);
			}
			f['size'] = 1;
			// fade to player color
			if (isAnt) {
				var color = this.meta['playercolors'][owner];
				if (conv > start) {
					ant.fade(Quality.LOW, 'r', 255, conv - 0.5, conv - 0.25);
					ant.fade(Quality.LOW, 'g', 255, conv - 0.5, conv - 0.25);
					ant.fade(Quality.LOW, 'b', 255, conv - 0.5, conv - 0.25);
				}
				ant.fade(Quality.LOW, 'r', color[0], conv - 0.25, conv);
				ant.fade(Quality.LOW, 'g', color[1], conv - 0.25, conv);
				ant.fade(Quality.LOW, 'b', color[2], conv - 0.25, conv);
			}
			// do moves
			var x = col;
			var y = row;
			if (isAnt) {
				for (var i = 0; i < orders.length; i++) {
					var dir = orders[i];
					this.turns[conv + i].clearFog(owner, y, x, this.rows,
							this.cols, this.parameters['viewradius2']);
					if (dir) {
						x += dir.x;
						y += dir.y;
						ant.fade(Quality.LOW, 'x', x, conv + i, conv + i + 0.5);
						ant.fade(Quality.LOW, 'y', y, conv + i, conv + i + 0.5);
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
									antObj.keyFrames[2]['x'] = antObj.keyFrames[1]['x'] + 0.5 * (offset['x'] * sq + (1 - sq) * nextDir.x);
									antObj.keyFrames[2]['y'] = antObj.keyFrames[1]['y'] + 0.5 * (offset['y'] * sq + (1 - sq) * nextDir.y);
									antObj.keyFrames[2].angle += 0.5 * angle;
								}
							}
						}*/
					}
				}
				if (end !== conv + orders.length) {
					// account for survivors
					this.turns[end - 1].clearFog(owner, y, x, this.rows,
							this.cols, this.parameters['viewradius2']);
				}
			}
			// end of life
			var collision = false;
			dir = collision ? end - 0.75 : end - 0.25;
			ant.fade(Quality.LOW, 'size', 0.0, dir, dir + 0.25);
			ant.fade(Quality.LOW, 'r', 0.0, dir, dir + 0.25);
			ant.fade(Quality.LOW, 'g', 0.0, dir, dir + 0.25);
			ant.fade(Quality.LOW, 'b', 0.0, dir, dir + 0.25);
			// account ant to the owner
			for (i = conv; i < end; i++) {
				this.turns[i].counts[owner]++;
			}
			for (i = Math.max(0, start); i < end; i++) {
				this.turns[i].ants.push(ant);
			}
			tl = lit.gimmeNext();
		}
		// score
		for (i = 0; i < this.players; i++) {
			var scores = tl.kw('s').as([DataType.SCORES]).params[0];
			setReplayDuration(scores.length - 1, false);
			for (var k = 0; k < scores.length; k++) {
				this.turns[k].scores[i] = scores[k];
			}
			for (; k < this.turns.length; k++) {
				this.turns[k].scores[i] = scores[scores.length - 1];
			}
			if (i != this.players - 1) tl = lit.gimmeNext();
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
	if (n >= this.players) {
		throw new Error('Player number ' + n + ' is out of range for per player parameter.');
	}
	return this.players[n];
};

/**
 * @class A highly optimized string tokenizer for replay files. It ignores blank
 *     lines and comment lines, trims and splits each line in two after the
 *     keyword. It processes a 220 KB file with over 27,000 lines in
 *     about 18 ms in Chromium on a 2,0 Ghz Core 2 Duo.
 * @constructor
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

/**
 * @constructor
 */
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

/**
 * @constructor
 */
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
