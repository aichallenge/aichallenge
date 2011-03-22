module ants;

import std.stdio;
import std.string;
import std.conv;
import std.algorithm;
import std.array;
import std.math;
import std.date;

enum SqVal : byte {
	WATER = -4, FOOD, LAND, DEAD,
	ME = 0, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z
}

struct Loc {
	uint row;
	uint col;
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
		uint _loadtime;
		uint _turntime;
		uint _cols;
		uint _rows;
		uint _turns;
		uint _viewradius2;
		uint _attackradius2;
		uint _spawnradius2;
		d_time _endtime;
		Loc[] _myAnts;
		Loc[] _enemyAnts;
		Loc[] _food;
		SqVal[][] _map;

		void setup(string data) {
			foreach (line; data.splitlines()) {
				line = line.strip().tolower();
				if (line.length > 0) {
					auto tokens = line.split();
					auto key = tokens[0];
					auto value = to!uint(tokens[1]);
					switch(key) {
						case "loadtime"     : _loadtime      = value; break;
						case "turntime"     : _turntime      = value; break;
						case "cols"         : _cols          = value; break;
						case "rows"         : _rows          = value; break;
						case "turns"        : _turns         = value; break;
						case "viewradius2"  : _viewradius2   = value; break;
						case "attackradius2": _attackradius2 = value; break;
						case "spawnradius2" : _spawnradius2  = value; break; 
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
			_myAnts.clear;
			_enemyAnts.clear;
			// update map and create new ant and food lists
			foreach (line; data.splitlines()) {
				line = line.strip();
				if (line.length > 0) {
					auto tokens = line.split();
					if (tokens.length >= 3) {
						auto row = to!uint(tokens[1]);
						auto col = to!uint(tokens[2]);
						switch(tokens[0].tolower) {
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
		uint loadtime() {
		return _loadtime;
		}

		/// time per turn in milliseconds
		uint turntime() {
		return _turntime;
		}

		/// height of the map
		uint rows() {
			return _rows;
		}

		/// width of the map
		uint cols() {
			return _cols;
		}

		/// maximum number of turns in the game
		uint turns() {
			return _turns;
		}

		/// view radius squared
		uint viewradius2() {
			return _viewradius2;
		}

		/// battle radius squared
		uint attackradius2() {
			return _attackradius2;
		}

		/// spawn radius squared
		uint spawnradius2() {
			return _spawnradius2;
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
		d_time timeRemaining() {
			d_time now = getUTCtime();
			return (now > _endtime) ? 0 : _endtime - now;
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
				currentLine.tolowerInPlace();
				switch (currentLine) {
					case "ready":
						ants.setup(mapData.data);
						ants.finishTurn();
						mapData = appender!(string)();
						break;
					case "go":
						ants._endtime = getUTCtime();
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