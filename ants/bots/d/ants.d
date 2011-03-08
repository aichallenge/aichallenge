module ants;

import std.stdio;
import std.string;
import std.conv;
import std.algorithm;
import std.array;
import std.math;

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
	void setup(uint rows, uint cols);
	void doTurn(Ants ants);
}

class Ants {

	private:
		Loc[] _myAnts;
		Loc[] _enemyAnts;
		Loc[] _foodList;
		uint _cols;
		uint _rows;
		SqVal[][] map;

		void setup(string data) {
			foreach (line; data.splitlines()) {
				line = line.strip().tolower();
				if (line.length > 0) {
					auto tokens = line.split();
					auto key = tokens[0];
					switch(key) {
						case "cols":
							_cols = to!uint(tokens[1]);
							break;
						case "rows":
							_rows = to!uint(tokens[1]);
							break;
						default:
					}
				}
			}
			map.length = _rows;
			foreach (ref row; map) {
				row.length = _cols;
				fill(row, SqVal.LAND);
			}
		}

		void update(string data) {
			// clear ant and food data
			foreach(row; map) {
				foreach(ref sq; row) {
					if (sq != SqVal.WATER) sq = SqVal.LAND;
				}
			}
			_foodList.clear;
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
								map[row][col] = owner;
								if (owner == SqVal.ME) {
									_myAnts ~= Loc(row, col);
								} else if (owner > SqVal.ME) {
									_enemyAnts ~= Loc(row, col);
								}
								break;
							case "f":
								map[row][col] = SqVal.FOOD;
								_foodList ~= Loc(row, col);
								break;
							case "w":
								map[row][col] = SqVal.WATER;
								break;
							case "d":
								map[row][col] = SqVal.DEAD;
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
		const(Loc)[] myAnts() {
			return _myAnts;
		}

		const(Loc)[] enemyAnts() {
			return _enemyAnts;
		}

		const(Loc)[] foodList() {
			return _foodList;
		}

		uint rows() {
			return _rows;
		}

		uint cols() {
			return _cols;
		}

		//returns the euclidean distance between two locations with the edges wrapped
		auto distance(const ref Loc loc1, const ref Loc loc2)
		{
			uint dr = abs(cast(int)(loc1.row - loc2.row));
			uint dc = abs(cast(int)(loc1.col - loc2.col));
			dr = min(dr, _rows - dr);
			dc = min(dc, _cols - dc);
			return sqrt(dr * dr + dc * dc);
		};

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

		bool passable(const ref Loc loc) {
			return map[loc.row][loc.col] != SqVal.WATER;
		}

		void issueOrder(const ref Loc loc, const ref Direction direction) {
			stdout.writefln("o %s %s %s", loc.row, loc.col, direction.key);
		}

		static void run(IBot bot) {
			Ants ants = new Ants();
			auto mapData = appender!(string)();
			foreach (currentLine; stdin.byLine()) {
				currentLine.tolowerInPlace();
				switch (currentLine) {
					case "ready":
						ants.setup(mapData.data);
						bot.setup(ants._rows, ants._cols);
						ants.finishTurn();
						mapData = appender!(string)();
						break;
					case "go":
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