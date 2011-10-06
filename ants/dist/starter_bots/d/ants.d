module ants;

import std.stdio;
import std.string;
import std.conv;
import std.algorithm;
import std.array;
import std.math;
import core.time;

enum SqVal : byte {
	WATER = -4, FOOD, LAND, DEAD,
	ME = 0, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z
}

struct Loc {
	uint row;
	uint col;
}

struct Hill {
	Loc loc;
	SqVal owner;
}

struct Direction {
	char key;
	int row;
	int col;
}

immutable Direction[4] AIM = [
	{'n', -1, 0},
	{'e', 0, 1},
	{'s', 1, 0},
	{'w', 0, -1}
];

interface IBot {
	void doTurn(Ants ants);
}

class Ants {

	private:
		uint _cols;
		uint _rows;
		long _loadtime;
		long _turntime;
		long _turns;
		long _viewradius2;
		long _attackradius2;
		long _spawnradius2;
		long _player_seed;
		TickDuration _endtime;
		Loc[] _food;
		Loc[] _myAnts;
		Loc[] _enemyAnts;
		Hill[] _myHills;
		Hill[] _enemyHills;
		SqVal[][] _map;

		void setup(string data) {
			foreach (line; data.splitLines()) {
				line = line.strip().toLower();
				if (line.length > 0) {
					auto tokens = line.split();
					auto key = tokens[0];
					auto value = to!long(tokens[1]);
					switch(key) {
						case "rows"         : _rows          = cast(uint)value; break;
						case "cols"         : _cols          = cast(uint)value; break;
						case "turns"        : _turns         = value; break;
						case "loadtime"     : _loadtime      = value; break;
						case "turntime"     : _turntime      = value; break;
						case "viewradius2"  : _viewradius2   = value; break;
						case "attackradius2": _attackradius2 = value; break;
						case "spawnradius2" : _spawnradius2  = value; break; 
						case "player_seed"  : _player_seed   = value; break; 
						default:
					}
				}
			}
			_map.length = _rows;
			foreach (ref row; _map) {
				row.length = _cols;
				fill(row, SqVal.LAND);
			}
		}

		void update(string data) {
			// clear ant and food data
			foreach(row; _map) {
				foreach(ref sq; row) {
					if (sq != SqVal.WATER) sq = SqVal.LAND;
				}
			}
			_food.clear;
			_myHills.clear;
			_enemyHills.clear;
			_myAnts.clear;
			_enemyAnts.clear;
			// update map and create new ant and food lists
			foreach (line; data.splitLines()) {
				line = line.strip();
				if (line.length > 0) {
					auto tokens = line.split();
					if (tokens.length >= 3) {
						auto row = to!uint(tokens[1]);
						auto col = to!uint(tokens[2]);
						switch(tokens[0].toLower) {
							case "h":
								SqVal owner = cast(SqVal) to!byte(tokens[3]);
								if (owner == SqVal.ME) {
									_myHills ~= Hill(Loc(row, col), owner);
								} else if (owner > SqVal.ME) {
									_enemyHills ~= Hill(Loc(row, col), owner);
								}
								break;
							case "a":
								SqVal owner = cast(SqVal) to!byte(tokens[3]);
								_map[row][col] = owner;
								if (owner == SqVal.ME) {
									_myAnts ~= Loc(row, col);
								} else if (owner > SqVal.ME) {
									_enemyAnts ~= Loc(row, col);
								}
								break;
							case "f":
								_map[row][col] = SqVal.FOOD;
								_food ~= Loc(row, col);
								break;
							case "w":
								_map[row][col] = SqVal.WATER;
								break;
							case "d":
								_map[row][col] = SqVal.DEAD;
								break;
							default:
						}
					}
				}
			}
		}

		void finishTurn() {
			stdout.writeln("go");
		}

	public:
		/// load time granted to a bot in milliseconds
		long loadtime() {
			return _loadtime;
		}

		/// time per turn in milliseconds
		long turntime() {
			return _turntime;
		}

		/// height of the map
		long rows() {
			return _rows;
		}

		/// width of the map
		long cols() {
			return _cols;
		}

		/// maximum number of turns in the game
		long turns() {
			return _turns;
		}

		/// view radius squared
		long viewradius2() {
			return _viewradius2;
		}

		/// battle radius squared
		long attackradius2() {
			return _attackradius2;
		}

		/// spawn radius squared
		long spawnradius2() {
			return _spawnradius2;
		}

		/// random number generator seed for the player
		long player_seed() {
			return _player_seed;
		}

		const(Hill)[] myHills() {
			return _myHills;
		}

		const(Hill)[] enemyHills() {
			return _enemyHills;
		}

		const(Loc)[] myAnts() {
			return _myAnts;
		}

		const(Loc)[] enemyAnts() {
			return _enemyAnts;
		}

		const(Loc)[] food() {
			return _food;
		}

		/// returns the euclidean distance between two locations with the edges wrapped
		auto distance(const ref Loc loc1, const ref Loc loc2) {
			uint dr = abs(cast(int)(loc1.row - loc2.row));
			uint dc = abs(cast(int)(loc1.col - loc2.col));
			dr = min(dr, _rows - dr);
			dc = min(dc, _cols - dc);
			return sqrt(dr * dr + dc * dc);
		}

		/// returns possible directions that go from one location to another on a torus
		Direction[] direction(const ref Loc loc1, const ref Loc loc2)
		body {
			Direction[] result;
			uint dr = abs(cast(int)(loc1.row - loc2.row));
			uint dc = abs(cast(int)(loc1.col - loc2.col));
			if (loc1.row > loc2.row && dr <= _rows - dr || loc1.row < loc2.row && dr >= _rows - dr) {
				result ~= AIM[0]; // north
			}
			if (loc1.col < loc2.col && dc <= _cols - dc || loc1.col > loc2.col && dc >= _cols - dc) {
				result ~= AIM[1]; // east
			}
			if (loc1.row < loc2.row && dr <= _rows - dr || loc1.row > loc2.row && dr >= _rows - dr) {
				result ~= AIM[2]; // south
			}
			if (loc1.col > loc2.col && dc <= _cols - dc || loc1.col < loc2.col && dc >= _cols - dc) {
				result ~= AIM[3]; // west
			}
			return result;
		}
		unittest {
			stdout.write("test direction calculation:");
			Ants ants = new Ants();
			ants._cols = 10;
			ants._rows = 10;
			assert(ants.direction(Loc(0,0), Loc(0,0)) == []);
			assert(ants.direction(Loc(0,0), Loc(1,0)) == [AIM[2]]);
			assert(ants.direction(Loc(0,0), Loc(0,1)) == [AIM[1]]);
			assert(ants.direction(Loc(0,0), Loc(1,1)) == [AIM[1], AIM[2]]);
			assert(ants.direction(Loc(1,1), Loc(0,0)) == [AIM[0], AIM[3]]);
			assert(ants.direction(Loc(0,0), Loc(9,9)) == [AIM[0], AIM[3]]);
			assert(ants.direction(Loc(0,0), Loc(5,5)) == [AIM[0], AIM[1], AIM[2], AIM[3]]);
			stdout.writeln(" ok");
		}

		/// checks if a square is not filled with water
		bool passable(const ref Loc loc) {
			return _map[loc.row][loc.col] != SqVal.WATER;
		}

		/// time remaining for the turn in milliseconds
		long timeRemaining() {
			TickDuration now = TickDuration.currSystemTick();
			return (now > _endtime) ? 0L : (_endtime - now).msecs;
		}

		/// adds a directional offset to a location and returns the result
		Loc destination(const ref Loc loc, const ref Direction d)
		in {
			assert(loc.col < _cols);
			assert(loc.row < _rows);
		}
		body {
			return Loc( (loc.row + d.row + _rows) % _rows, 
			            (loc.col + d.col + _cols) % _cols );
		}
		unittest {
			stdout.write("test destination calculation:");
			Ants ants = new Ants();
			ants._cols = 10;
			ants._rows = 10;
			// go north across boundary
			assert(ants.destination(Loc(0, 5), AIM[0]) == Loc(9, 5));
			// go east across boundary
			assert(ants.destination(Loc(5, 9), AIM[1]) == Loc(5, 0));
			// go south across boundary
			assert(ants.destination(Loc(9, 5), AIM[2]) == Loc(0, 5));
			// go west across boundary
			assert(ants.destination(Loc(5, 0), AIM[3]) == Loc(5, 9));
			// invalid location
			try {
				ants.destination(Loc(9873237, 113), AIM[3]);
			} catch {
				// this is correct
			}
			stdout.writeln(" ok");
		}

		/// allows the bot to issue an order from a location into any direction
		void issueOrder(const ref Loc loc, const ref Direction direction) {
			stdout.writefln("o %s %s %s", loc.row, loc.col, direction.key);
		}

		/// creates a new Ants instance and runs the given bot in it 
		static void run(IBot bot) {
			auto ants = new Ants();
			auto mapData = appender!(string)();
			foreach (currentLine; stdin.byLine()) {
				currentLine.toLowerInPlace();
				switch (currentLine) {
					case "ready":
						ants.setup(mapData.data);
						ants.finishTurn();
						mapData = appender!(string)();
						break;
					case "go":
						ants._endtime = TickDuration.currSystemTick(); 
						ants._endtime += TickDuration.from!"msecs"(ants._turntime);
						ants.update(mapData.data);
						bot.doTurn(ants);
						ants.finishTurn();
						mapData = appender!(string)();
						break;
					default:
						mapData.put(currentLine);
						mapData.put('\n');
				}
			}
		}

}
