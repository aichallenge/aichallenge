const DRAW_INTERVAL = 50;       // setInterval value
const SANDS = 5;                // count of background textures
const TIME_PER_TURN = 250;      // in milli seconds


// Animation keyframe for ants
var KeyFrame = function(time, x, y, angle, r, g, b, alpha) {
	this.time = time;
	this.x = x;
	this.y = y;
	this.angle = angle % (2 * Math.PI);
	this.jitter = 0;
	this.r = r;
	this.g = g;
	this.b = b;
	this.alpha = alpha;
};
KeyFrame.prototype.copyForTurn = function(turn, width, height) {
	result = new KeyFrame(turn, this.x, this.y, this.angle, this.r, this.g, this.b, this.alpha);
	result.x = (result.x + width) % width;
	result.y = (result.y + height) % height;
	return result;
};

const foodColor = [ 200, 200, 150, 1.0 ];
const playerColors = [
	[ 255,   0,   0, 1.0 ], [   0,   0, 255, 1.0 ], [   0, 255,   0, 1.0 ], [ 255, 255,   0, 1.0 ],
	[ 255, 255, 255, 1.0 ], [ 255, 255, 255, 1.0 ], [ 255, 255, 255, 1.0 ], [ 255, 255, 255, 1.0 ],
	[ 255, 255, 255, 1.0 ], [ 255, 255, 255, 1.0 ], [ 255, 255, 255, 1.0 ], [ 255, 255, 255, 1.0 ],
	[ 255, 255, 255, 1.0 ], [ 255, 255, 255, 1.0 ], [ 255, 255, 255, 1.0 ], [ 255, 255, 255, 1.0 ],
	[ 255, 255, 255, 1.0 ], [ 255, 255, 255, 1.0 ], [ 255, 255, 255, 1.0 ], [ 255, 255, 255, 1.0 ],
	[ 255, 255, 255, 1.0 ], [ 255, 255, 255, 1.0 ], [ 255, 255, 255, 1.0 ], [ 255, 255, 255, 1.0 ],
	[ 255, 255, 255, 1.0 ], [ 255, 255, 255, 1.0 ]
];


// A constructor for on-screen ant objects
var Ant = function(id) {
	this.id = id;
};
// Initializes the ant's animation keyframes with default
Ant.prototype.init = function(turn, x, y) {
	var angle = Math.random() * 2 * Math.PI;
	var color = foodColor;
	this.keyFrames = [
		new KeyFrame(turn - 1, x, y, angle, color[0], color[1], color[2], color[3]),
		new KeyFrame(turn    , x, y, angle, color[0], color[1], color[2], color[3])
	];
	return this;
};
// uses the last animation frame as start and end for next turn'a animation
Ant.prototype.copyForNextTurn = function(width, height) {
	var frame = this.keyFrames[this.keyFrames.length - 1];
	var result = new Ant(this.id);
	result.keyFrames = [
		frame.copyForTurn(this.keyFrames[0].time + 1, width, height),
		frame.copyForTurn(this.keyFrames[0].time + 2, width, height)
	];
	return result;
};
// fades an attribute from the first keyframe to 'value' in the last
Ant.prototype.fade = function(key, value) {
	this.keyFrames[this.keyFrames.length - 1][key] = value;
	if (this.keyFrames.length > 2) {
		var timea = this.keyFrames[0].time;
		for (var i = 1; i < this.keyFrames.length - 1; i++) {
			var p = this.keyFrames[i].time - timea;
			this.keyFrames[i][key] = (1.0 - p) * this.keyFrames[0][key] + p * value;
		}
	}
};
// returns an object with values interpolated along the given time
Ant.prototype.interpolate = function(time) {
	for (var a = this.keyFrames.length - 1; a >= 0; a--) {
		if (this.keyFrames[a].time <= time) {
			var b = a + 1;
			var posb = (time - this.keyFrames[a].time) / (this.keyFrames[b].time - this.keyFrames[a].time);
			var posa = 1.0 - posb;
			break;
		}
	}
	var kfa = this.keyFrames[a];
	var kfb = this.keyFrames[b];
	var angle = posa * kfa.angle + posb * kfb.angle;
	if (kfa.angle - kfb.angle > Math.PI) {
		angle -= posa * 2 * Math.PI;
	} else if (kfb.angle - kfa.angle > Math.PI) {
		angle -= posb * 2 * Math.PI;
	}
	result = new KeyFrame(
		time,
		posa * kfa.x       + posb * kfb.x,
		posa * kfa.y       + posb * kfb.y,
		angle,
		(posa * kfa.r  + posb * kfb.r) | 0,
		(posa * kfa.g  + posb * kfb.g) | 0,
		(posa * kfa.b  + posb * kfb.b) | 0,
		posa * kfa.alpha  + posb * kfb.alpha
	);
	result.jitter = posa * kfa.jitter + posb * kfb.jitter;
	result.id   = this.id;
	result.type = this.type;
	return result;
};
// animates the ant ([{<time between 0 and 1>, {<attribute to set absolute>, ...}, {<attribute to set relative>, ...}}, ...])
Ant.prototype.animate = function(list) {
	var interpol = new Array(list.length);
	for (var i = 0; i < list.length; i++) {
		var time = this.keyFrames[0].time + list[i].time;
		interpol[i] = this.interpolate(time);
	}
	for (i = 0; i < list.length; i++) {
		for (var a = 0; a < this.keyFrames.length; a++) {
			if (this.keyFrames[a].time > time) {
				this.keyFrames.splice(a, 0, interpol[i]);
				break;
			}
		}
		for (var key in list[i].absolute) {
			interpol[i][key] = list[i].absolute[key];
		}
		for (var key in list[i].relative) {
			interpol[i][key] += list[i].relative[key];
		}
	}
};


// A coordinate set
var Pos = function(x, y, angle) {
	this.x = x;
	this.y = y;
	this.angle = angle;
};


// A constructor for turns
var Turn = function() {
	this.spawns = [];
	this.conversions = [];
	this.deaths = [];
	this.existing = [];
	this.orders = [];
};
Turn.prototype.food = function(data, id, prev) {
	// check for food item refresh
	for (var eid in this.existing) {
		if (this.existing[eid].x == data.x && this.existing[eid].y == data.y && this.existing[eid].player === undefined) {
			return id;
		}
	}
	if (prev !== undefined) {
		prev.spawns.push({ id: id, x: data.x, y: data.y });
	}
	this.existing.push({ id: id, x: data.x, y: data.y });
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
	prev.conversions.push({ id: target.id, player: data.player });
};
Turn.prototype.kill = function(data, prev) {
	var deathList = [];
	for (var id in this.existing) {
		if (this.existing[id].x == data.x && this.existing[id].y == data.y) {
			if (this.existing[id].player === undefined) {
				alert('food killed');
			}
			deathList.push(this.existing[id].id);
			delete this.existing[id];
		}
	}
	if (deathList.length == 0) {
		throw 'Nothing to kill at (' + data.x + ';' + data.y + ')';
	}
	prev.deaths.push(deathList);
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

// translate compass directions to movement offsets
const directions = {
	N : new Pos( 0, -1, 0 / 4 * Math.PI),
	NE: new Pos(+1, -1, 1 / 4 * Math.PI),
	E : new Pos(+1,  0, 2 / 4 * Math.PI),
	SE: new Pos(+1, +1, 3 / 4 * Math.PI),
	S : new Pos( 0, +1, 4 / 4 * Math.PI),
	SW: new Pos(-1, +1, 5 / 4 * Math.PI),
	W : new Pos(-1,  0, 6 / 4 * Math.PI),
	NW: new Pos(-1, -1, 7 / 4 * Math.PI)
};


const ParameterType = {
	NONE: 0,
	STRING: 1,
	UINT: 2,
	SCORES: 3,
	LOCATION: 4,
	LOCATION_PLAYER: 5,
	LOCATION_NSEW: 6
};


// the main 'application' object
var visualizer = {

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

	replay: {
		version: undefined,
		parameters: undefined,
		players: undefined,		// array of players (names, submission ids)
		parser: {
			lines: undefined,
			line: undefined
		},
		walls: undefined,
		turns: undefined,
		winner: undefined,

		// finds the next line that isn't empty or a comment line
		nonEmptyLine: function(canEnd) {
			var result;
			do {
				this.parser.line++;
				if (this.parser.line >= this.parser.lines.length) {
					if (canEnd) {
						return;
					} else {
						throw 'The input ended unexpectedly';
					}
				}
				result = this.parser.lines[this.parser.line];
				// 'trim'
				result = result.replace(/^\s\s*/, '').replace(/\s\s*$/, '');
			} while (result == '' || result.charAt(0) == '#');
			return result;
		},

		parseLocation: function(tokens, result) {
			result.y = parseInt(tokens[0]);
			if (isNaN(result.y) || result.y < 0 || result.y >= this.parameters.rows) {
				throw 'Y is not an integer within map borders';
			}
			result.x = parseInt(tokens[1]);
			if (isNaN(result.x) || result.x < 0 || result.x >= this.parameters.cols) {
				throw 'X is not an integer within map borders';
			}
		},

		// splits the currently selected line into tokens and checks if the parameter count is valid
		tokenize: function(line, parameterTypes, allowOthers) {
			// get command type first
			var sp = line.indexOf(' ');
			if (sp == -1) {
				sp = line.length;
			}
			var result = {
				type: line.substr(0, sp).toLowerCase()
			};
			line = line.substr(sp).replace(/^\s\s*/, '').replace(/\s\s*$/, '');
			// parse the rest
			var i;
			var tokens = line.split(' ');
			var parameterType = parameterTypes[result.type];
			if (parameterType === undefined) {
				if (!allowOthers) {
					tokens = [];
					for (var parameterType in parameterTypes) {
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
				case ParameterType.LOCATION_NSEW:
					if (tokens.length != 3) {
						throw 'Location and direction expected';
					}
					this.parseLocation(tokens, result);
					result.direction = directions[tokens[2].toUpperCase()];
					if (result.direction === undefined) {
						throw 'Direction ' + tokens[2] + ' is undefined';
					}
					break;
			}
			return result;
		},

		load: function(replayStr) {
			var row, col, i, a, g, k, t, x, y, c;
			var id = 0;      // 'auto-increment' value for ant ids
			var tturns = []; // for each turn, contains a basic data structure containing all the information from <replayStr>
			var t = 0;       // keeps track of the current turn, during parsing
			var trimmed;     // an input line without comments and leading or trailing whitespace
			var result;      // a tokenized input line
			var validTokens; // tokens understood in the current context (depending on player count)
			var playerId;
			var endTurn = false;
			var obj;
			var constraints, base, tile;
			var command;
			var old;
			var antObj;
			var spawn, order, offset, angle, frame, copy, conversion, deathList, death;

			// convert replayStr into tokens per line
			this.parser.lines = replayStr.split('\n');
			this.parser.line = -1;
			try {
				// check version
				trimmed = this.nonEmptyLine();
				this.version = this.tokenize(trimmed, { version: ParameterType.UINT }).value;
				if (this.version < 1 || this.version > 1) {
					throw 'File version ' + this.version + ' cannot be read by this visualizer';
				}
				// turns
				trimmed = this.nonEmptyLine();
				// initialization turn '0'
				if (this.tokenize(trimmed, { turn: ParameterType.UINT }).value != t) {
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
								id = tturns[0].food({ x: col, y: this.walls.length }, id);
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
								this.players[i] = { name: 'Player ' + (i + 1) };
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
					d   : ParameterType.LOCATION,        // ant died last round
					o   : ParameterType.LOCATION_NSEW,   // move order
					turn: ParameterType.UINT,
					end : ParameterType.NONE
				};
				// other turns
				this.turns.push({ ants: [], counts: new Array(this.players.length) });
				while (!endTurn) {
					t++;
					if (result.type == 'turn' && result.value != t) {
						throw 'Expected turn ' + t;
					} else if (result.type == 'end') {
						endTurn = true;
					}
					tturns.push(new Turn());
					tturns[t].carry(tturns[t - 1], this.parameters.cols, this.parameters.rows);
					this.turns.push({ ants: [], counts: new Array(this.players.length) });
					trimmed = this.nonEmptyLine();
					result = this.tokenize(trimmed, { score: ParameterType.SCORES });
					this.turns[t].scores = result.scores.slice(0);
					while (result.type != 'turn' && result.type != 'end') {
						trimmed = this.nonEmptyLine(endTurn);
						if (trimmed === undefined) { // this is the end of file check
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
				alert(error + '\nIn line ' + (this.parser.line + 1) + ': ' + this.parser.lines[this.parser.line]);
			}
			
			// Add ants...
			var nextAntDirection = function(id, turn) {
				for (var nadk = 0; nadk < tturns[turn + 1].orders.length; nadk++) {
					var nadAction = tturns[turn + 1].orders[nadk];
					if (nadAction.id == id) {
						return nadAction.direction;
					}
				}
			}
			var nextDir;
			// we need the existing dummy food items from turn '0'
			var antStates = {};
			for (id in tturns[0].existing) {
				spawn = tturns[0].existing[id];
				antObj = new Ant(spawn.id).init(0, spawn.x, spawn.y);
				antStates[antObj.id] = antObj;
			}
			for (t = 0; t < this.turns.length - 1; t++) {
				// movement
				for (i = 0; i < tturns[t].orders.length; i++) {
					order = tturns[t].orders[i];
					antObj = antStates[order.id];
					offset = order.direction;
					antObj.keyFrames[1].jitter = 0.08;
					antObj.keyFrames[1].x = Math.round(antObj.keyFrames[0].x) + offset.x;
					antObj.keyFrames[1].y = Math.round(antObj.keyFrames[0].y) + offset.y;
					antObj.keyFrames[1].angle = offset.angle;
					if (antObj.keyFrames[1].x == antObj.keyFrames[0].x + offset.x && antObj.keyFrames[1].y == antObj.keyFrames[0].y + offset.y) {
						antObj.keyFrames[0].jitter = 0.08;
					} else {
						antObj.keyFrames[0].jitter = 0;
					}
					antObj.animate([{
						time: 0.5,
						absolute: {
							x: antObj.keyFrames[1].x,
							y: antObj.keyFrames[1].y,
							angle: offset.angle
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
					antObj = new Ant(spawn.id).init(t, spawn.x, spawn.y);
					if (spawn.player !== undefined) {
						nextDir = nextAntDirection(antObj.id, t);
						if (nextDir) {
							antObj.keyFrames[1].angle = nextDir.angle;
						}
					}
					antObj.keyFrames[0].alpha = 0;
					antObj.animate([{ time: 0.75, absolute: { alpha: 0 }, relative: {} }]);
					this.turns[t].ants.push(antObj);
					antStates[antObj.id] = antObj;
				}
				// conversion
				for (i = 0; i < tturns[t].conversions.length; i++) {
					conversion = tturns[t].conversions[i];
					antObj = antStates[conversion.id];
					nextDir = nextAntDirection(antObj.id, t);
					if (nextDir) {
						antObj.keyFrames[1].angle = nextDir.angle;
					}
					antObj.animate([{ time: 0.75, absolute: {}, relative: {} }]);
					var color = playerColors[conversion.player];
					antObj.keyFrames[2].r = color[0];
					antObj.keyFrames[2].g = color[1];
					antObj.keyFrames[2].b = color[2];
				}
				// kills
				for (i = 0; i < tturns[t].deaths.length; i++) {
					deathList = tturns[t].deaths[i];
					if (deathList.length > 1) {
						// collision
						for (k = 0; k < deathList.length; k++) {
							antObj = antStates[deathList[k]];
							antObj.fade('alpha', -1);  // I hope this hack works consistently across browsers
							delete antStates[deathList[0]];
						}
					} else {
						antObj = antStates[deathList[0]];
						antObj.fade('alpha', 0);
						antObj.animate([{ time: 0.50, absolute: { alpha: 1 }, relative: {} }]);
						antObj.animate([{ time: 0.75, absolute: { alpha: 0 }, relative: {} }]);
						delete antStates[deathList[0]];
					}
				}
				// Copy the last ant state to the next level
				for (i in antStates) {
					copy = antStates[i].copyForNextTurn(this.parameters.cols, this.parameters.rows);
					antStates[i] = copy;
					this.turns[t + 1].ants.push(copy);
					this.turns[t + 1].counts[antStates[i].type]++;
				}
			}
		}
	},

	init: function() {
		var i, parameters, equalPos, parameter, value;
		this.canvas = document.getElementById('canvas');
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
		// check for game_id in the URL
		parameters = window.location.href.split('?');
		if (parameters.length > 1) {
			parameters = parameters[1].split('&');
			for (i = 0; i < parameters.length; i++) {
				equalPos = parameters[i].indexOf('=');
				parameter = parameters[i].substr(0, equalPos);
				value = parameters[i].substr(equalPos + 1);
				this.parameters[parameter] = value;
			}
		}
		if (this.parameters.game_id === undefined) {
			// get list of replays
			var buttonList = document.getElementById('buttonList');
			buttonList.appendChild(document.createTextNode('Stored replays:'));
			buttonList.appendChild(document.createElement('br'));
			var p = new XMLHttpRequest();
			p.open("GET", 'replays', false);
			p.setRequestHeader('Cache-Control', 'no-cache');
			p.send(null);
			var rows = p.responseText.split('<A HREF="');
			for (i = rows.length - 2; i >= 3; i--) {
				var filename = rows[i].substr(0, rows[i].indexOf('.replay'));
				var button = document.createElement('input');
				var url = 'replays/' + filename + '.replay';
				button.setAttribute('type', 'button');
				button.setAttribute('value', unescape(filename));
				button.setAttribute('onClick', 'visualizer.load(\'' + url + '\')');
				buttonList.appendChild(button);
				buttonList.appendChild(document.createElement('br'));
			}
			buttonList.children[1].click();
		} else {
			visualizer.load('replays/' + this.parameters.game_id + '.replay');
		}
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
		this.scale = Math.min(10, Math.max(1, Math.min(window.innerWidth / this.replay.parameters.cols, window.innerHeight / this.replay.parameters.rows))) | 0;
		var pw = this.scale * this.replay.parameters.cols;
		var ph = this.scale * this.replay.parameters.rows;
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
		this.intervalDraw = window.setInterval('visualizer.draw()', DRAW_INTERVAL);
	},

	load: function(what) {
		window.clearInterval(this.intervalDraw);
		this.intervalDraw = undefined;
		this.timeOffset = undefined;
		this.turn = undefined;
		var replayStr;
		if (what === undefined) {
			replayStr = document.getElementById('txtReplay').value;
			document.title = 'Ant Canvas - playing pasted string';
		} else {
			var p = new XMLHttpRequest();
			p.open("GET", what, false);
			p.send(null);
			replayStr = p.responseText;
			while (what.indexOf('/') != -1) {
				what = what.substr(what.indexOf('/') + 1);
			}
			what = unescape(what.substr(0, what.indexOf('.')));
			document.title = 'Ant Canvas - playing "' + what + '"';
		}
		this.replay.load(replayStr);
		this.initWaitForImages();
	},

	draw: function() {
		//~ var timeA = new Date().getTime();
		//ctx2d.globalCompositeOperation = 'copy';
		//ctx2d.globalCompositeOperation = 'source-over';
		var time = new Date().getTime();
		var drawOrder = new Array();
		var transition;
		if (this.timeOffset === undefined) {
			this.timeOffset = time;
			transition = true;
		}
		this.ctx2d.drawImage(this.canvasBackground, 0, 0);
		var turn;
		if (this.intervalDraw === undefined) {
			turn = this.turn;
			time = turn - 1;
			transition = true;
		} else {
			var time = (time - this.timeOffset) / TIME_PER_TURN;
			var anim;
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
			document.getElementById('lblTurn').innerHTML = ((turn > this.replay.turns.length - 2) ? 'end result' : turn + ' / ' + (this.replay.turns.length - 2));
		}
		for (var i = 0; i < this.replay.turns[turn].ants.length; i++) {
			var antObj = this.replay.turns[turn].ants[i].interpolate(time);
			drawOrder.push(antObj);
		}
		this.ctx2dBackground.globalAlpha = 100 / TIME_PER_TURN;
		if (Math.random() < 0.5) {
			this.ctx2dBackground.strokeStyle = '#222';
		} else {
			this.ctx2dBackground.strokeStyle = '#CA7';
		}
		this.ctx2dBackground.beginPath();
		this.ctx2d.textBaseline = 'top';
		for (var n = 0; n < drawOrder.length; n++) {
			var antObj = drawOrder[n];
			this.ctx2d.save();
			if (true) {
				this.ctx2d.fillStyle = 'rgba(' + antObj.r + ', ' + antObj.g + ', ' + antObj.b + ', ' + antObj.alpha + ')';
				this.ctx2d.fillRect((this.scale * antObj.x) | 0, (this.scale * antObj.y) | 0, this.scale, this.scale);
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
			var time = (new Date().getTime() - this.timeOffset) / TIME_PER_TURN;
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
				this.timeOffset = new Date().getTime() - (this.turn - 1) * TIME_PER_TURN;
			}
			this.turn = undefined;
			this.intervalDraw = window.setInterval('visualizer.draw()', DRAW_INTERVAL);
			this.playdir = 0;
		} else {
			window.clearInterval(this.intervalDraw);
			this.intervalDraw = undefined;
			var time = (new Date().getTime() - this.timeOffset) / TIME_PER_TURN;
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
