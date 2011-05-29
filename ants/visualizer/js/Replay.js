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
function Replay(replay, debug) {
	var i, k, scores;
	/**
	 * @private
	 */
	this.debug = debug;
	// check for a replay from the pre-json era and convert it.
	if (replay.search(/^\s*{/) === -1) {
		replay = this.txtToJson(replay);
	} else {
		replay = JSON.parse(replay);
	}
	// check if we have meta data or just replay data
	var format = 'json';
	if (replay['challenge'] === undefined) {
		this.meta = {
			'challenge': 'ants',
			'replayformat': format,
			'replaydata': replay
		};
	} else {
		this.meta = replay;
		if (typeof this.meta['replaydata'] == 'string') {
			format = 'storage';
			this.meta['replaydata'] = this.txtToJson(this.meta['replaydata']);
		}
		replay = this.meta['replaydata'];
	}
	// validate metadata
	if (this.meta['challenge'] !== 'ants') {
		throw new Error('This visualizer is for the ants challenge,'
				+ ' but a "' + this.meta['challenge']
				+ '" replay was loaded.');
	} else if (this.meta['replayformat'] !== format) {
		throw new Error('Replays in the format "' + this.meta['replayformat']
				+ '" are not supported.');
	}
	if (!replay) {
		throw new Error('replay meta data is no object notation');
	}
	// start parsing process
	this.duration = 0;
	var that = this;
	if (replay) {
		var stack = [];
		var keyEq = function(obj, key, val) {
			if (obj[key] !== val && !that.debug) {
				throw new Error(stack.join('.') + '.' + key + ' should be ' + val + ', but was found to be ' + obj[key] + '!');
			}
		};
		var keyRange = function(obj, key, min, max) {
			if (!(obj[key] >= min && (obj[key] <= max || max === undefined)) && !that.debug) {
				throw new Error(stack.join('.') + '.' + key + ' should be within [' + min + ' .. ' + max + '], but was found to be ' + obj[key] + '!');
			}
		};
		var keyIsArr = function(obj, key, minlen, maxlen) {
			if (!(obj[key] instanceof Array)) {
				throw new Error(stack.join('.') + '.' + key + ' should be an array, but was found to be of type ' + typeof obj[key] + '!');
			}
			stack.push(key);
			keyRange(obj[key], 'length', minlen, maxlen);
			stack.pop();
		};
		var keyIsStr = function(obj, key, minlen, maxlen) {
			if (typeof obj[key] !== 'string') {
				throw new Error(stack.join('.') + '.' + key + ' should be a string, but was found to be of type ' + typeof obj[key] + '!');
			}
			stack.push(key);
			keyRange(obj[key], 'length', minlen, maxlen);
			stack.pop();
		};
		var keyOption = function(obj, key, func, params) {
			if (obj[key] !== undefined) {
				func.apply(undefined, [obj, key].concat(params));
			}
		};
		var keyDefault = function(obj, key, def, func, params) {
			if (obj[key] === undefined) {
				obj[key] = def;
			}
			func.apply(undefined, [obj, key].concat(params));
		};
		var enterObj = function(obj, key) {
			if (!(obj[key] instanceof Object)) {
				throw new Error(stack.join('.') + '.' + key + ' should be an object, but was found to be of type ' + typeof obj[key] + '!');
			}
			stack.push(key);
			return obj[key];
		};
		var durationSetter = null;
		var setReplayDuration = function(duration, fixed) {
			if (durationSetter) {
				if (!fixed && that.duration < duration || fixed && that.duration !== duration && !that.debug) {
					throw new Error('Replay duration was previously set to ' + that.duration + ' by "' + durationSetter + '" and is now redefined to be ' + duration);
				}
			} else {
				that.duration = duration;
				if (fixed) durationSetter = obj;
			}
		};
		enterObj(this.meta, 'replaydata');
		keyEq(replay, 'revision', 2);
		keyRange(replay, 'players', 1, 26);
		this.players = replay['players'];
		keyOption(replay, 'viewradius2', keyRange, [0, undefined]);
		// map
		var map = enterObj(replay, 'map');
		keyIsArr(map, 'data', 1, undefined);
		stack.push('data');
		keyIsStr(map['data'], 0, 1, undefined);
		stack.pop();
		keyDefault(map, 'rows', map['data'].length, keyEq, [map['data'].length]);
		this.rows = map['rows'];
		keyDefault(map, 'cols', map['data'][0].length, keyEq, [map['data'][0].length]);
		this.cols = map['cols'];
		var mapdata = enterObj(map, 'data');
		this.walls = new Array(mapdata.length);
		var regex = /[^%*.a-z]/;
		for (var r = 0; r < mapdata.length; r++) {
			keyIsStr(mapdata, r, map['cols'], map['cols']);
			var maprow = new String(mapdata[r]);
			if ((i = maprow.search(regex)) !== -1 && !this.debug) {
				throw new Error('Invalid character "' + maprow.charAt(i) + '" in map. Zero based row/col: ' + r + '/' + i)
			}
			this.walls[r] = new Array(maprow.length);
			for (var c = 0; c < maprow.length; c++) {
				this.walls[r][c] = (maprow.charAt(c) === '%');
			}
		}
		stack.pop();
		stack.pop();
		// ants
		keyIsArr(replay, 'ants', 0, undefined);
		stack.push('ants');
		var ants = replay['ants'];
		regex = /[^nsew-]/;
		for (var n = 0; n < ants.length; n++) {
			keyIsArr(ants, n, 4, 7);
			stack.push(n);
			var obj = ants[n];
			// row must be within map height
			keyRange(obj, 0, 0, map['rows'] - 1);
			// col must be within map width
			keyRange(obj, 1, 0, map['cols'] - 1);
			// start must be >= 0
			keyRange(obj, 2, 0, undefined);
			if (obj[2] === 0) {
				// conversion must be >= 0
				keyRange(obj, 3, 0, undefined);
			} else {
				// conversion must be > start
				keyRange(obj, 3, obj[2] + 1, undefined);
			}
			if (obj.length > 4) {
				// end turn must be > conversion turn
				keyRange(obj, 4, obj[3] + 1, undefined);
				// player index must match player count
				keyRange(obj, 5, 0, this.players - 1);
				// moves must be valid
				var lifespan = obj[4] - obj[3];
				keyIsStr(obj, 6, lifespan - 1, lifespan);
				setReplayDuration(obj[4] - 1, obj[6].length !== lifespan);
				if ((i = obj[6].search(regex)) !== -1 && !this.debug) {
					throw new Error('Invalid character "' + obj[6].charAt(i) + '" in move orders at index ' + i + ' in the string "' + obj[6] + '"');
				}
			} else {
				setReplayDuration(obj[3] - 1, false);
			}
			stack.pop();
		}
		// scores
		keyIsArr(replay, 'scores', this.players, this.players);
		stack.push('scores');
		var scoreslist = replay['scores'];
		for (i = 0; i < this.players; i++) {
			setReplayDuration(scoreslist[i].length - 1, false);
		}
		if (replay['bonus']) {
			keyIsArr(replay, 'bonus', this.players, this.players);
		}
		// prepare score and count lists
		this.turns = new Array(this.duration + 1);
		this.scores = new Array(this.duration + 1);
		this.counts = new Array(this.duration + 1);
		this.fogs = new Array(this.players);
		for (n = 0; n <= this.duration; n++) {
			this.scores[n] = new Array(this.players);
			this.counts[n] = new Array(this.players);
			for (i = 0; i < this.players; i++) this.counts[n][i] = 0;
		}
		for (i = 0; i < this.players; i++) {
			scores = scoreslist[i];
			for (k = 0; k < scores.length; k++) {
				this.scores[k][i] = scores[k];
			}
			for (; k <= this.duration; k++) {
				this.scores[k][i] = scores[scores.length - 1];
			}
			this.fogs[i] = new Array(this.duration + 1);
		}
		for (i = 0; i < ants.length; i++) {
			if (ants[i][5] !== undefined) {
				// account ant to the owner
				for (n = ants[i][3]; n < ants[i][4]; n++) {
					this.counts[n][ants[i][5]]++;
				}
			}
		}
		this.aniAnts = new Array(ants.length);
	}
	// add missing meta data
	if (!(this.meta['playernames'] instanceof Array)) {
		if (this.meta['players'] instanceof Array) {
			// move players to playernames in old replays
			this.meta['playernames'] = this.meta['players'];
			delete this.meta['players'];
		} else {
			this.meta['playernames'] = new Array(this.players);
		}
	}
	if (!(this.meta['playercolors'] instanceof Array)) {
		this.meta['playercolors'] = new Array(this.players);
	}
	for (i = 0; i < this.players; i++) {
		if (!this.meta['playernames'][i]) {
			this.meta['playernames'][i] = 'player ' + (i + 1);
		}
		if (!(this.meta['playercolors'][i] instanceof Array)) {
			this.meta['playercolors'][i] = PLAYER_COLORS[i];
		}
	}
	this.htmlPlayerColors = new Array(this.players);
	for (i = 0; i < this.players; i++) {
		this.htmlPlayerColors[i] = '#';
		this.htmlPlayerColors[i] += INT_TO_HEX[this.meta['playercolors'][i][0]];
		this.htmlPlayerColors[i] += INT_TO_HEX[this.meta['playercolors'][i][1]];
		this.htmlPlayerColors[i] += INT_TO_HEX[this.meta['playercolors'][i][2]];
	}
}
Replay.prototype.txtToJson = function(replay) {
	var i, c, lit, tl, args, rows, cols, owner, row, col, isAnt, conv, end;
	var orders, fixed, scores, result, isReplay;
	lit = new LineIterator(replay);
	result = {
		'revision': 2,
		'map': {'data': []},
		'ants': [],
		'scores': []
	};
	this.turns = [];
	try {
		// version check
		tl = lit.gimmeNext();
		isReplay = tl.keyword === 'v';
		if (isReplay) {
			tl.kw('v').as([DataType.IDENT, DataType.POSINT]);
			tl.expectEq(0, 'ants'); // game name
			tl.expectEq(1, 1);      // file version
			// players
			tl = lit.gimmeNext();
			tl.kw('players').as([DataType.POSINT]);
			tl.expectLE(0, 26);     // player count <= 26
			result['players'] = tl.params[0];
			// parameters
			tl = lit.gimmeNext();
		}
		while (tl.keyword !== 'm') {
			args = [DataType.STRING];
			if (tl.keyword === 'viewradius2' || tl.keyword === 'rows' || tl.keyword === 'cols' || tl.keyword === 'players') {
				args[0] = DataType.UINT;
			}
			tl.as(args);
			if (tl.keyword === 'rows' || tl.keyword === 'cols') {
				result['map'][tl.keyword] = tl.params[0];
			} else {
				result[tl.keyword] = tl.params[0];
			}
			tl = lit.gimmeNext();
		}
		// map
		cols = undefined;
		rows = 0;
		do {
			tl.as([DataType.STRING]);
			if (cols === undefined) {
				cols = tl.params[0].length;
			} else if (tl.params[0].length !== cols && !this.debug) {
				throw new Error('Map lines have different lenghts');
			}
			result['map']['data'].push(tl.params[0]);
			if (!isReplay) {
				// in a map file we want to extract starting positions
				for (i = 0; i < cols; i++) {
					c = tl.params[0].charAt(i);
					if (c >= 'a' && c <= 'z') {
						result['ants'].push([rows, i, 0, 0, 1, c.toUpperCase().charCodeAt(0) - 65, '-']);
					} else if (c === '*') {
						result['ants'].push([rows, i, 0, 0]);
					}
				}
			}
			rows++;
			if (isReplay || lit.moar()) {
				tl = lit.gimmeNext();
			} else {
				break;
			}
		} while (tl.keyword === 'm');
		// food / ant
		if (isReplay) {
			while (tl.keyword === 'a') {
				//     row            col            start          conversion
				tl.as([DataType.UINT, DataType.UINT, DataType.UINT, DataType.UINT,
						DataType.UINT, DataType.UINT, DataType.STRING], 3);
				//		end            owner          orders            # optional
				row = tl.params[0];
				if (row >= this.rows) throw new Error('Row exceeds map width.');
				col = tl.params[1];
				if (col >= this.cols) throw new Error('Col exceeds map height.');
				conv = tl.params[3];
				end = tl.params[4];
				if (end === undefined) end = conv;
				owner = tl.params[5];
				isAnt = owner !== undefined;
				if (isAnt && owner >= this.players) {
					throw new Error('Player index out of range.');
				}
				if (tl.params.length === 6) {
					tl.params.push('');
				}
				orders = tl.params[6];
				if (isAnt) {
					fixed = orders.length !== end - conv;
					if (fixed && orders.length + 1 !== end - conv) {
						throw new Error('Number of orders does not match life span.');
					}
				}
				result['ants'].push(tl.params);
				tl = lit.gimmeNext();
			}
			// score
			for (i = 0; i < result['players']; i++) {
				scores = tl.kw('s').as([DataType.SCORES]).params[0];
				result['scores'].push(scores);
				if (i != result['players'] - 1) tl = lit.gimmeNext();
			}
		} else {
			for (i = 0; i < result['players']; i++) {
				result['scores'].push([0]);
			}
		}
		if (lit.moar()) {
			tl = lit.gimmeNext();
			throw new Error('Extra data at end of file.');
		}
	} catch (error) {
		error.message = tl.line + '\n' + error.message;
		throw error;
	}
	return result;
};
Replay.prototype.getTurn = function(n) {
	var i, turn, ants, ant, aniAnt, lastFrame, dead;
	if (this.turns[n] === undefined) {
		if (n !== 0) this.getTurn(n - 1);
		turn = this.turns[n] = [];
		// generate ants & keyframes
		ants = this.meta['replaydata']['ants'];
		for (i = 0; i < ants.length; i++) {
			ant = ants[i];
			if (ant[2] === n + 1 || n === 0 && ant[2] === 0) {
				// spawn this ant
				aniAnt = this.aniAnts[i] = new Ant(i, ant[2] - 0.25);
				aniAnt.owner = ant[5];
				var f = aniAnt.frameAt(ant[2] - 0.25, Quality.LOW, false);
				f['x'] = ant[1];
				f['y'] = ant[0];
				f['r'] = FOOD_COLOR[0];
				f['g'] = FOOD_COLOR[1];
				f['b'] = FOOD_COLOR[2];
				if (ant[2] !== 0) {
					f = aniAnt.frameAt(ant[2], Quality.LOW, true);
					f['size'] = 1.0;
					f = aniAnt.frameAt(ant[2] + 0.125, Quality.LOW, true);
					f['size'] = 1.5;
					f = aniAnt.frameAt(ant[2] + 0.25, Quality.LOW, true);
					f['size'] = 0.7;
					f = aniAnt.frameAt(ant[2] + 0.5, Quality.LOW, true);
				}
				f['size'] = 1;
			} else if (this.aniAnts[i]) {
				// load existing state
				aniAnt = this.aniAnts[i];
			} else {
				// continue with next ant
				continue;
			}
			if (ant[5] !== undefined && (ant[3] === n + 1 || n === 0 && ant[3] === 0)) {
				// fade to player color
				var color = this.meta['playercolors'][ant[5]];
				if (ant[3] != ant[2]) {
					aniAnt.fade(Quality.LOW, 'r', 255, ant[3] - 0.5, ant[3] - 0.25);
					aniAnt.fade(Quality.LOW, 'g', 255, ant[3] - 0.5, ant[3] - 0.25);
					aniAnt.fade(Quality.LOW, 'b', 255, ant[3] - 0.5, ant[3] - 0.25);
					aniAnt.frameAt(ant[3] - 0.25, Quality.LOW, false)['owner'] = ant[5];
				}
				aniAnt.fade(Quality.LOW, 'r', color[0], ant[3] - 0.25, ant[3]);
				aniAnt.fade(Quality.LOW, 'g', color[1], ant[3] - 0.25, ant[3]);
				aniAnt.fade(Quality.LOW, 'b', color[2], ant[3] - 0.25, ant[3]);
			}
			if (ant[6] !== undefined && n >= ant[3] && n < ant[3] + ant[6].length) {
				// move
				aniAnt.frameAt(n, Quality.LOW, true)['owner'] = ant[5];
				var dir = undefined;
				switch (ant[6].charAt(n - ant[3])) {
					case 'n':
					case 'N':
						dir = Directions.N;
						break;
					case 'e':
					case 'E':
						dir = Directions.E;
						break;
					case 's':
					case 'S':
						dir = Directions.S;
						break;
					case 'w':
					case 'W':
						dir = Directions.W;
				}
				if (dir) {
					lastFrame = aniAnt.lo[aniAnt.lo.length - 1];
					aniAnt.fade(Quality.LOW, 'x', lastFrame['x'] + dir.x, n, n + 0.5);
					aniAnt.fade(Quality.LOW, 'y', lastFrame['y'] + dir.y, n, n + 0.5);
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
//			if (ant[4] !== ant[3] + ant[6].length) {
				// account for survivors
//					this.turns[obj[4] - 1].clearFog(obj[5], y, x, this.rows,
//							this.cols, replay['viewradius2']);
//			}
			dead = (ant[4] || ant[3]);
			if (dead === n + 1) {
				// end of life
				aniAnt.fade(Quality.LOW, 'size', 0.0, dead - 0.25, dead);
				aniAnt.fade(Quality.LOW, 'r'   , 0.0, dead - 0.25, dead);
				aniAnt.fade(Quality.LOW, 'g'   , 0.0, dead - 0.25, dead);
				aniAnt.fade(Quality.LOW, 'b'   , 0.0, dead - 0.25, dead);
			}
			if (n < dead) {
				// assign ant to display list
				turn.push(aniAnt);
			}
		}
	}
	return this.turns[n];
};
Replay.prototype.getFog = function(player, turn) {
	var i, fogs, fog, row, col, radius, radius2, row_wrap, col_wrap;
	var fog_row, fog_row1, fog_row2, aniAnts, aniAnt;
	fogs = this.fogs[player];
	if (fogs[turn] === undefined) {
		fogs[turn] = fog = new Array(this.rows);
		for (row = 0; row < this.rows; row++) {
			fog[row] = new Array(this.cols);
			for (col = 0; col < this.cols; col++) {
				fog[row][col] = true;
			}
		}
		radius2 = this.meta['replaydata']['viewradius2'];
		radius = Math.sqrt(radius2) | 0;
		aniAnts = this.getTurn(turn);
		for (i = 0; i < aniAnts.length; i++) {
			aniAnt = aniAnts[i].interpolate(turn, Quality.LOW);
			if (aniAnt && aniAnt['owner'] === player) {
				row_wrap = new Array(2 * radius + 1);
				for (row = 2 * radius; row >= 0; row--) {
					row_wrap[row] = aniAnt['y'] - radius + row;
					row_wrap[row] -= Math.floor(row_wrap[row] / this.rows) * this.rows;
				}
				col_wrap = new Array(2 * radius + 1);
				for (col = 2 * radius; col >= 0; col--) {
					col_wrap[col] = aniAnt['x'] - radius + col;
					col_wrap[col] -= Math.floor(col_wrap[col] / this.cols) * this.cols;
				}
				col = col_wrap[radius];
				for (row = 1; row <= radius; row++) {
					fog[row_wrap[radius - row]][col] = false;
					fog[row_wrap[radius + row]][col] = false;
				}
				fog_row = fog[row_wrap[radius]];
				for (col = 0; col < col_wrap.length; col++) {
					fog_row[col_wrap[col]] = false;
				}
				for (row = 1; row <= radius; row++) {
					fog_row1 = fog[row_wrap[radius - row]];
					fog_row2 = fog[row_wrap[radius + row]];
					for (col = 1; col <= radius; col++) {
						if (row*row + col*col <= radius2) {
							fog_row1[col_wrap[radius - col]] = false;
							fog_row1[col_wrap[radius + col]] = false;
							fog_row2[col_wrap[radius - col]] = false;
							fog_row2[col_wrap[radius + col]] = false;
						}
					}
				}
			}
		}
	}
	return fogs[turn];
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
