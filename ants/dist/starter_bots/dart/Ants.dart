#library("ants");

#import("lib/node/node.dart");

// Write to stderr because stdin and stdout are in use.

void log(String msg) {
  console.error(msg);
}

interface Bot {
  void onReady();
  void onTurn();
  void onEnd();
}

class Config {
  int rows;
  int cols;
  int loadtime;
  int turntime;
  int turns;
  int viewradius2;
  int attackradius2;
  int spawnradius2;
  int player_seed;
  int food_rate;
  int food_turn;
  int food_start;
  
  void set(String key, String value) {
    switch(key) {
      case 'rows': rows = parseInt(value); break;
      case 'cols': cols = parseInt(value); break;
      case 'loadtime': loadtime = parseInt(value); break;
      case 'turntime': turntime = parseInt(value); break;
      case 'viewradius2': viewradius2 = parseInt(value); break;
      case 'attackradius2': attackradius2 = parseInt(value); break;
      case 'spawnradius2': spawnradius2 = parseInt(value); break;
      case 'player_seed': player_seed = parseInt(value); break;
      case 'food_rate': food_rate = parseInt(value); break;
      case 'food_turn': food_turn = parseInt(value); break;
      case 'food_start': food_start = parseInt(value); break;
      default: // ignore
    }
  }
}

class Tile {
  int type;
  Tile(int this.type);
}

class Ant extends Tile {
  int row;
  int col;
  int owner;
  Ant(this.row, this.col, this.owner) : super(Ants.ANT);
}

class Food extends Tile {
  int row;
  int col;
  Food(int this.row, int this.col) : super(Ants.FOOD);
}

class Dead extends Tile {
  int row;
  int col;
  int owner;
  Dead(int this.row, int this.col, this.owner) : super(Ants.DEAD);
}

class Hill {
  int row;
  int col;
  int owner;
  Hill(int this.row, int this.col, int this.owner);
}

class Order {
  int row;
  int col;
  String direction;
  Order(int this.row, int this.col, String this.direction);
}

class Ants {
	Bot bot;
	int currentTurn;
	Config config;
	List<List<Tile>> map;
	List<Order> orders;
	List<Ants> ants;
	List<Food> food;
	List<Hill> hills;
	List<Dead> dead;
	bool vision;
	bool visionOffsets;

  // landtypes	
	final static int LAND = 0;
	final static int DEAD = 1;
	final static int ANT = 2;
	final static int WATER = 3;
	final static int FOOD = 4;
	
	Ants() {
	  currentTurn = -1;
	  vision = false;
	  visionOffsets = false;
	  config = new Config();
	  map = new List<List<Tile>>();
	  orders = new List<Order>();
	  ants = new List<Ants>();
	}
	
	void start(Bot botInput) {
		this.bot = botInput;
		String partialline = "";
		process.stdin.resume();
		process.stdin.setEncoding('utf8');
		process.stdin.on('data', (String chunk) {
			List<String> lines = chunk.split("\n");
			lines[0] = partialline + lines[0];
			partialline = "";
			// Complete lines will leave an empty
			// string at the end, if that is not the case
			// buffer this line until the next chunk
			if (lines[lines.length - 1] !== "") {
				partialline = lines.removeLast();
			}
			for (int i = 0, len = lines.length; i < len; ++i) {
				processLine(lines[i]);
			}
		});
	}

	void processLine(String inLine) {
		this.vision = false;
		String trimmed = inLine.trim();
		List<String> line = trimmed.split(' ');

		if (line[0] === 'ready') {
			Tile land = new Tile(LAND);
		  this.map = new List<List<Tile>>(this.config.rows);
			for (int row = 0; row < this.config.rows; ++row) {
				for (int col = 0; col < this.config.cols; ++col) {
					if (col === 0) {
						this.map[row] = new List<Tile>(this.config.cols);
					}
					this.map[row][col] = land;
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
				Tile land = new Tile(LAND);
				if (this.currentTurn == 1) {
				  // Create Map
				  for (int row = 0; row < config.rows; row++) {
				    map[row] = new List<Tile>(config.cols);
				    for (int col = 0; col < config.cols; col++) {
				      map[row][col] = land;
				    }
				  }
				} else {
  				//Reset map except for water:
  				for (int row = 0, rlen = this.map.length; row < rlen; ++row) {
  					for (int col = 0, clen = this.map[row].length; col < clen; ++col) {
  						if (this.map[row][col].type !== WATER) {
  							this.map[row][col] = land;
  						}
  					}
  				}
				}
				this.hills = new List<Hill>();
				this.ants = new List<Ants>();
				this.food = new List<Food>();
				this.dead = new List<Dead>();
			}
		} else {
			if (this.currentTurn === 0 && line[0] !== 'ready') {
			  this.config.set(line[0], line[1]);
			} else {
				if (line[0] === 'w') {
  				int row = parseInt(line[1]);
  				int col = parseInt(line[2]);
					this.map[row][col] = new Tile(WATER);
				} else if (line[0] === 'f') {
  				int row = parseInt(line[1]);
  				int col = parseInt(line[2]);
  				Food food = new Food(row, col);
					this.map[row][col] = food;
					this.food.addLast(food);
				} else {
					if (line[0] === 'a') {
    				int row = parseInt(line[1]);
    				int col = parseInt(line[2]);
  					int owner = parseInt(line[3]);
  					Ant ant = new Ant(row, col, owner);
						this.map[row][col] = ant;
						this.ants.addLast(ant);
					} else if (line[0] === 'd') {
    				int row = parseInt(line[1]);
    				int col = parseInt(line[2]);
  					int owner = parseInt(line[3]);
  					Dead dead = new Dead(row, col, owner);
						if (this.map[row][col].type !== LAND) {
							this.map[row][col] = dead;
						}
						this.dead.addLast(dead);
					} else  if (line[0] === 'h') {
    				int row = parseInt(line[1]);
    				int col = parseInt(line[2]);
  					int owner = parseInt(line[3]);
						this.hills.addLast(new Hill(row, col, owner));
					}
				}
			}
		}
	}

	void issueOrder(int row, int col, String direction) {
		this.orders.addLast(new Order(row, col, direction));
	}
	
	void finishTurn() {
		for (int i = 0, len = this.orders.length; i < len; ++i) {
			Order order = this.orders[i];
			fs.writeSync(process.stdout.fd, 'o '+order.row+' '+order.col+' '+order.direction+'\n');
		}
		this.orders = new List<Order>();
		fs.writeSync(process.stdout.fd,'go\n');
		process.stdout.flush();
	}
	
	Tile tileInDirection(int row, int col, String direction) {
		int rowd = 0;
		int cold = 0;
		if (direction === 'N') {
			rowd = -1;
		} else if (direction === 'E') {
			cold = 1;
		} else if (direction === 'S') {
			rowd = 1;
		} else if (direction === 'W') {
			cold = -1;
		}
		int newrow = row + rowd;
		int newcol = col + cold;
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
	}
	
	List<Hill> myHills() {
		List<Hill> result = new List<Hill>();
		for (int i = 0, len = this.hills.length; i < len; ++i) {
			Hill hill = this.hills[i];
			if (hill.owner === 0) {
				result.addLast(hill);
			}
		}
		return result;
	}
	
	List<Hill> enemyHills() {
		List<Hill> result = new List<Hill>();
		for (int i = 0, len = this.hills.length; i < len; ++i) {
			Hill hill = this.hills[i];
			if (hill.owner !== 0) {
				result.addLast(hill);
			}
		}
		return result;
	}
	
	List<Ant> myAnts() {
		List<Ant> result = new List<Ant>();
		for (int i = 0, len = this.ants.length; i < len; ++i) {
			Ant ant = this.ants[i];
			if (ant.owner === 0) {
				result.addLast(ant);
			}
		}
		return result;
	}
	
	List<Ant> enemyAnts() {
		List<Ant> result = [];
		for (int i = 0, len = this.ants.length; i < len; ++i) {
			Ant ant = this.ants[i];
			if (ant.owner !== 0) {
				result.addLast(ant);
			}
		}
		return result;
	}
	
	bool passable(int row, int col) {
		return (this.map[row][col].type !== WATER);
	}
	
	bool unoccupied(int row, int col) {
		return (this.map[row][col].type === LAND ||
				this.map[row][col].type === DEAD);
	}

	List<int> destination(int row, int col, String direction) {
		int rowd = 0;
		int cold = 0;
		if (direction === 'N') {
			rowd = -1;
		} else if (direction === 'E') {
			cold = 1;
		} else if (direction === 'S') {
			rowd = 1;
		} else if (direction === 'W') {
			cold = -1;
		}
		int newrow = row + rowd;
		int newcol = col + cold;
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
	}

	double distance(int fromRow, int fromCol, int toRow, int toCol) {
		var dr = Math.min(Math.abs(fromRow - toRow), this.config.rows - Math.abs(fromRow - toRow));
		var dc = Math.min(Math.abs(fromCol - toCol), this.config.cols - Math.abs(fromCol - toCol));
		return Math.sqrt((dr * dr) + (dc * dc));
	}
	
	List<String> direction(int fromRow, int fromCol, int toRow, int toCol) {
		List<String> d = new List<String>();
		fromRow = fromRow % this.config.rows;
		toRow = toRow % this.config.rows;
		fromCol = fromCol % this.config.cols;
		toCol = toCol % this.config.cols;

		if (fromRow < toRow) {
			if (toRow - fromRow >= this.config.rows/2) {
				d.addLast('N');
			}
			if (toRow - fromRow <= this.config.rows/2) {
				d.addLast('S');
			}
		} else if (toRow < fromRow) {
			if (fromRow - toRow >= this.config.rows/2) {
				d.addLast('S');
			}
			if (fromRow - toRow <= this.config.rows/2) {
				d.addLast('N');
			}
		}

		if (fromCol < toCol) {
			if (toCol - fromCol >= this.config.cols/2) {
				d.addLast('W');
			}
			if (toCol - fromCol <= this.config.cols/2) {
				d.addLast('E');
			}
		} else if (toCol < fromCol) {
			if (fromCol - toCol >= this.config.cols/2) {
				d.addLast('E');
			}
			if (fromCol - toCol <= this.config.cols/2) {
				d.addLast('W');
			}
		}
		return d;
	}
	
	var visible(int row, int col) {
		if (this.vision === false || !this.vision || this.vision.length === 0) {
			this.vision = [];
			if (this.visionOffsets === false) {
				this.visionOffsets = [];
				var mx = Math.floor(Math.sqrt(this.config.viewradius2));
				for (var dRow = -mx; dRow < mx+1; ++dRow) {
					for (var dCol = -mx; dCol < mx+1; ++dCol) {
						var d = Math.pow(dRow, 2) + Math.pow(dCol, 2);
						if (d <= this.config.viewradius2) {
							this.visionOffsets.addLast([dRow, dCol]);
						}
					}
				}
			}

			for (int trow = 0; trow < this.config.rows; ++trow) {
				for (int tcol = 0; tcol < this.config.cols; ++tcol) {
					if (tcol === 0) {
						this.vision[trow] = [];
					}
					this.vision[trow][tcol] = false;
				}
			}
			var myAnts = this.myAnts();
			for (int antI = 0; antI < myAnts.length; antI++) {
				var ant = myAnts[antI];
				for (int visionOffsetI= 0; visionOffsetI < this.visionOffsets.length; visionOffsetI++) {
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
}

int parseInt(String s) {
  // Why isn't this in the standard library?
  int sign = 1;
  int value = 0;
  int start = 0;
  if (s.length < 1) {
    throw new Exception("Empty string");
  }
  if (s[0] == '-') {
    start = 1;
    sign = -1;
  }
  for (int i = start; i < s.length; i++) {
    int c = s.charCodeAt(i) - 48;
    if (c >= 0 && c <= 9) {
      value = value * 10 + c;
    } else {
      throw new Exception("couldn't parse integer ${s}");
    }
  }
  return sign * value;
}
