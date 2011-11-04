var fs = require('fs')

exports.ants = {
	'bot': null,
	'currentTurn': -1,
	'config': {},
	'map': [],
	'orders': [],
	'ants': [],
	'food': [],
	'vision': false,
	'visionOffsets': false,
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
			for (var i = 0, len = lines.length; i < len; ++i) {
				thisoutside.processLine(lines[i]);
			}
		});
	},
	'processLine': function(line) {
		this.vision = false;
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
				for (var row = 0, rlen = this.map.length; row < rlen; ++row) {
					for (var col = 0, clen = this.map[row].length; col < clen; ++col) {
						if (this.map[row][col].type !== this.landTypes.WATER) {
							this.map[row][col] = {'type': this.landTypes.LAND};
						}
					}
				}
				this.hills = [];
				this.ants = [];
				this.food = [];
				this.dead = [];
			}
		} else {
			if (this.currentTurn === 0 && line[0] !== 'ready') {
				this.config[line[0]] = line[1];
			} else {
				var row = parseInt(line[1]);
				var col = parseInt(line[2]);
				var obj = { 'row': row, 'col': col };
				if (line[0] === 'w') {
					this.map[row][col] = {'type': this.landTypes.WATER, 'data': {}};
				} else if (line[0] === 'f') {
					this.map[row][col] = {'type': this.landTypes.FOOD, 'data': {}};
					this.food.push(obj);
				} else {
					var owner = parseInt(line[3]);
					obj['owner'] = owner;
					if (line[0] === 'a') {
						this.map[row][col] = {'type': this.landTypes.ANT, 'data': {
							'owner': owner
						}};
						this.ants.push(obj);
					} else if (line[0] === 'd') {
						if (this.map[row][col] !== this.landTypes.LAND) {
							this.map[row][col] = {'type': this.landTypes.DEAD, 'data': {
								'owner': owner
							}};
						}
						this.dead.push(obj)
					} else  if (line[0] === 'h') {
						this.hills.push(obj);
					}
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
		for (var i = 0, len = this.orders.length; i < len; ++i) {
			var order = this.orders[i];
			fs.writeSync(process.stdout.fd, 'o '+order.row+' '+order.col+' '+order.direction+'\n');
		}
		this.orders = [];
		fs.writeSync(process.stdout.fd,'go\n');
		process.stdout.flush();
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
	'myHills': function() {
		var result = [];
		for (var i = 0, len = this.hills.length; i < len; ++i) {
			var hill = this.hills[i];
			if (hill.owner === 0) {
				result.push(hill);
			}
		}
		return result;
	},
	'enemyHills': function() {
		var result = [];
		for (var i = 0, len = this.hills.length; i < len; ++i) {
			var hill = this.hills[i];
			if (hill.owner !== 0) {
				result.push(hill);
			}
		}
		return result;
	},
	'myAnts': function() {
		var result = [];
		for (var i = 0, len = this.ants.length; i < len; ++i) {
			var ant = this.ants[i];
			if (ant.owner === 0) {
				result.push(ant);
			}
		}
		return result;
	},
	'enemyAnts': function() {
		var result = [];
		for (var i = 0, len = this.ants.length; i < len; ++i) {
			var ant = this.ants[i];
			if (ant.owner !== 0) {
				result.push(ant);
			}
		}
		return result;
	},
	'passable': function(row, col) {
		return (this.map[row][col].type !== this.landTypes.WATER);
	},
	'unoccupied': function(row, col) {
		return (this.map[row][col].type === this.landTypes.LAND ||
				this.map[row][col].type === this.landTypes.DEAD);
	},
	'destination': function(row, col, direction) {
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
		return [newrow, newcol];
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
	},
	'visible': function(row, col) {
		if (this.vision === false || !this.vision || this.vision.length === 0) {
			this.vision = [];
			if (this.visionOffsets === false) {
				this.visionOffsets = [];
				var mx = Math.floor(Math.sqrt(this.config.viewradius2));
				for (var dRow = -mx; dRow < mx+1; ++dRow) {
					for (var dCol = -mx; dCol < mx+1; ++dCol) {
						var d = Math.pow(dRow, 2) + Math.pow(dCol, 2);
						if (d <= this.config.viewradius2) {
							this.visionOffsets.push([dRow, dCol]);
						}
					}
				}
			}
			
			for (var trow = 0; trow < this.config.rows; ++trow) {
				for (var tcol = 0; tcol < this.config.cols; ++tcol) {
					if (tcol === 0) {
						this.vision[trow] = [];
					}
					this.vision[trow][tcol] = false;
				}
			}
			var myAnts = this.myAnts();
			for (var antI in myAnts) {
				var ant = myAnts[antI];
				for (var visionOffsetI in this.visionOffsets) {
					var vo = this.visionOffsets[visionOffsetI];
					var visionRow = ant.row + vo[0];
					var visionCol = ant.col + vo[1];
					if (visionRow < 0) {
						visionRow = (this.config.rows-1) + visionRow;
					} else if (visionRow >= this.config.rows) {
						visionRow = visionRow - this.config.rows;
					}
					if (visionCol < 0) {
						visionCol = (this.config.cols-1) + visionCol;
					} else if (visionCol >= this.config.cols) {
						visionCol = visionCol - this.config.cols;
					}
					this.vision[visionRow][visionCol] = true;
				}
			}
		}
		return this.vision[row][col];
	}
};
