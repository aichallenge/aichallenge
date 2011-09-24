exports.ants = {
	'bot': null,
	'currentTurn': -1,
	'config': {},
	'map': [],
	'orders': [],
	'ants': [],
	'food': [],
	'landTypes': {
		'LAND': 0,
		'DEAD': 1,
		'ANT': 2,
		'WATER': 3,
		'FOOD': 4
	},
	'start': function(botInput) {
		this.bot = botInput;
		process.stdin.resume();
		process.stdin.setEncoding('utf8');
		var thisoutside = this;
		process.stdin.on('data', function(chunk) {
			var lines = chunk.split("\n");
			for (var i in lines) {
				thisoutside.processLine(lines[i]);
			}
		});
	},
	'processLine': function(line) {
		line = line.trim().split(' ');

		if (line[0] === 'ready') {
			for (var row = 0; row < this.config.rows; ++row) {
				for (var col = 0; col < this.config.cols; ++col) {
					if (col === 0) {
						this.map[row] = [];
					}
					this.map[row][col] = {'type': this.landTypes.LAND};
				}
			}
			this.bot.onReady();
			return;
		} else if(line[0] === 'go') {
			this.bot.onTurn();
			return;
		} else if(line[0] === 'end') {
			this.bot.onEnd();
			return;
		}
		if (line[0] === 'turn') {
			this.currentTurn = parseInt(line[1]);
			if (this.currentTurn > 0) {
				//Reset map except for water:
				for (var row in this.map) {
					for (var col in this.map[row]) {
						if (this.map[row][col].type !== this.landTypes.WATER) {
							this.map[row][col] = {'type': this.landTypes.LAND};
						}
					}
				}
				this.ants = [];
				this.food = [];
			}
		} else {
			if (this.currentTurn === 0 && line[0] !== 'ready') {
				this.config[line[0]] = line[1];
			} else {
				if (line[0] === 'w') {
					this.map[parseInt(line[1])][parseInt(line[2])] = {'type': this.landTypes.WATER, 'data': {}};
				} else if (line[0] === 'f') {
					this.map[parseInt(line[1])][parseInt(line[2])] = {'type': this.landTypes.FOOD, 'data': {}};
					this.food.push({
						'row': parseInt(line[1]),
						'col': parseInt(line[2])
					});
				} else if (line[0] === 'r') {
					this.map[parseInt(line[1])][parseInt(line[2])] = {'type': this.landTypes.LAND, 'data': {}};//Introduce new landtype?
				} else if (line[0] === 'a') {
					this.map[parseInt(line[1])][parseInt(line[2])] = {'type': this.landTypes.ANT, 'data': {
						'owner': parseInt(line[3])
					}};
					this.ants.push({
						'row': parseInt(line[1]),
						'col': parseInt(line[2]),
						'owner': parseInt(line[3])
					});
				} else if (line[0] === 'd') {
					this.map[parseInt(line[1])][parseInt(line[2])] = {'type': this.landTypes.DEAD, 'data': {
						'owner': parseInt(line[3])
					}};
				}
			}
		}
	},
	'issueOrder': function(row, col, direction) {
		this.orders.push({
			'row': parseInt(row),
			'col': parseInt(col),
			'direction': direction
		});
	},
	'finishTurn': function() {
		for (var i in this.orders) {
			var order = this.orders[i];
			console.log('o '+order.row+' '+order.col+' '+order.direction);
		}
		this.orders = [];
		console.log('go');
	},
	'tileInDirection': function(row, col, direction) {
		var rowd = 0;
		var cold = 0;
		if (direction === 'N') {
			rowd = -1;
		} else if (direction === 'E') {
			cold = 1;
		} else if (direction === 'S') {
			rowd = 1;
		} else if (direction === 'W') {
			cold = -1;
		}
		var newrow = row + rowd;
		var newcol = col + cold;
		if (newrow < 0) {
			newrow = this.config.rows-1;
		} else if (newrow > this.config.rows-1) {
			newrow = 0;
		}
		if (newcol < 0) {
			newcol = this.config.cols-1;
		} else if (newcol > this.config.cols-1) {
			newcol = 0;
		}
		return this.map[newrow][newcol];
	},
	'myAnts': function() {
		var result = [];
		for (var i in this.ants) {
			if (this.ants[i].owner === 0) {
				result.push(this.ants[i]);
			}
		}
		return result;
	},
	'enemyAnts': function() {
		var result = [];
		for (var i in this.ants) {
			if (this.ants[i].owner !== 0) {
				result.push(this.ants[i]);
			}
		}
		return result;
	},
	'passable': function(row, col, direction) {
		return (this.tileInDirection(row, col, direction).type !== this.landTypes.WATER);
	},
	'distance': function(fromRow, fromCol, toRow, toCol) {
		var dr = Math.min(Math.abs(fromRow - toRow), this.config.rows - Math.abs(fromRow - toRow));
		var dc = Math.min(Math.abs(fromCol - toCol), this.config.cols - Math.abs(fromCol - toCol));
		return Math.sqrt((dr * dr) + (dc * dc));
	},
	'direction': function(fromRow, fromCol, toRow, toCol) {
		var d = [];
		fromRow = fromRow % this.config.rows;
		toRow = toRow % this.config.rows;
		fromCol = fromCol % this.config.cols;
		toCol = toCol % this.config.cols;
		
		if (fromRow < toRow) {
			if (toRow - fromRow >= this.config.rows/2) {
				d.push('N');
			}
			if (toRow - fromRow <= this.config.rows/2) {
				d.push('S');
			}
		} else if (toRow < fromRow) {
			if (fromRow - toRow >= this.config.rows/2) {
				d.push('S');
			}
			if (fromRow - toRow <= this.config.rows/2) {
				d.push('N');
			}
		}
		
		if (fromCol < toCol) {
			if (toCol - fromCol >= this.config.cols/2) {
				d.push('W');
			}
			if (toCol - fromCol <= this.config.cols/2) {
				d.push('E');
			}
		} else if (toCol < fromCol) {
			if (fromCol - toCol >= this.config.cols/2) {
				d.push('E');
			}
			if (fromCol - toCol <= this.config.cols/2) {
				d.push('W');
			}
		}
		return d;
	}
};
