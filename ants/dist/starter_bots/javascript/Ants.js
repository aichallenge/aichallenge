ants = {
	'bot': null,
	'currentTurn': -1,
	'config': {},
	'map': [],
	'orders': [],
	'ants': [],
	'landTypes': {
		'LAND': 0,
		'DEAD': 1,
		'ANT': 2,
		'WATER': 3,
		'FOOD': 4
	},
	'start': function(botInput) {
		exports.bot = botInput;
		process.stdin.resume();
		process.stdin.setEncoding('utf8');
		process.stdin.on('data', function(chunk) {
			var lines = chunk.split("\n");
			for (var i in lines) {
				exports.processLine(lines[i]);
			}
		});
	},
	'processLine': function(line) {
		line = line.trim();

		if (line == 'ready') {
			for (var row = 0; row < exports.config.rows; ++row) {
				for (var col = 0; col < exports.config.cols; ++col) {
					if (col == 0) {
						exports.map[row] = [];
					}
					exports.map[row][col] = {'type': exports.landTypes.LAND};
				}
			}
			exports.bot.onReady();
			return;
		} else if(line == 'go') {
			exports.bot.onTurn();
			return;
		} else if(line == 'end') {
			exports.bot.onEnd();
			return;
		}
		if (line.substring(0,5) == 'turn ') {
			exports.currentTurn = parseInt(line.substring(5));
			if (exports.currentTurn > 0) {
				//Reset map except for water:
				for (var row in exports.map) {
					for (var col in exports.map[row]) {
						if (exports.map[row][col].type != exports.landTypes.WATER) {
							exports.map[row][col] = {'type': exports.landTypes.LAND};
						}
					}
				}
				exports.ants = [];
			}
		} else {
			if (exports.currentTurn == 0 && line != 'ready') {
				var configline = line.split(' ');
				exports.config[configline[0]] = configline[1];
			} else {
				var dataline = line.split(' ');
				if (dataline[0] == 'w') {
					exports.map[parseInt(dataline[1])][parseInt(dataline[2])] = {'type': exports.landTypes.WATER, 'data': {}};
				} else if (dataline[0] == 'f') {
					exports.map[parseInt(dataline[1])][parseInt(dataline[2])] = {'type': exports.landTypes.FOOD, 'data': {}};
				} else if (dataline[0] == 'r') {
					exports.map[parseInt(dataline[1])][parseInt(dataline[2])] = {'type': exports.landTypes.LAND, 'data': {}};//Introduce new landtype?
				} else if (dataline[0] == 'a') {
					exports.map[parseInt(dataline[1])][parseInt(dataline[2])] = {'type': exports.landTypes.ANT, 'data': {
						'owner': parseInt(dataline[3])
					}};
					exports.ants.push({
						'row': parseInt(dataline[1]),
						'col': parseInt(dataline[2]),
						'owner': parseInt(dataline[3])
					});
				} else if (dataline[0] == 'd') {
					exports.map[parseInt(dataline[1])][parseInt(dataline[2])] = {'type': exports.landTypes.DEAD, 'data': {
						'owner': parseInt(dataline[3])
					}};
				}
			}
		}
	},
	'issueOrder': function(row, col, direction) {
		exports.orders.push({
			'row': parseInt(row),
			'col': parseInt(col),
			'direction': direction
		});
	},
	'finishTurn': function() {
		for (var i in exports.orders) {
			var order = exports.orders[i];
			console.log('o '+order.row+' '+order.col+' '+order.direction);
		}
		exports.orders = [];
		console.log('go');
	},
	'tileInDirection': function(row, col, direction) {
		var rowd = 0;
		var cold = 0;
		if (direction == 'N') {
			rowd = -1;
		} else if (direction == 'E') {
			cold = 1;
		} else if (direction == 'S') {
			rowd = 1;
		} else if (direction == 'W') {
			cold = -1;
		}
		var newrow = row + rowd;
		var newcol = col + cold;
		if (newrow < 0) {
			newrow = exports.config.rows-1;
		} else if (newrow > exports.config.rows-1) {
			newrow = 0;
		}
		if (newcol < 0) {
			newcol = exports.config.cols-1;
		} else if (newcol > exports.config.cols-1) {
			newcol = 0;
		}
		return exports.map[newrow][newcol];
	}
};
exports.currentTurn = ants.currentTurn;
exports.start = ants.start;
exports.processLine = ants.processLine;
exports.landTypes = ants.landTypes;
exports.config = ants.config;
exports.map = ants.map;
exports.issueOrder = ants.issueOrder;
exports.finishTurn = ants.finishTurn;
exports.orders = ants.orders;
exports.ants = ants.ants;
exports.tileInDirection = ants.tileInDirection;