module MyBot;

import ants;
import std.stdio     : stdout;

class RandomBot : IBot {

	bool[][] moveMap;
	
	void setup(uint rows, uint cols) {
		moveMap.length = rows;
		foreach (ref row; moveMap) {
			row.length = cols;
		}
	}

	void doTurn(Ants engine) {
		// consider all squares free
		foreach (ref row; moveMap) foreach (ref sq; row) sq = true;
		foreach (antLoc; engine.myAnts()) {
			// try all directions until one is passable and not occupied
			bool blocked = true;
			foreach(direction; AIM) {
				auto antGo = engine.destination(antLoc, direction);
				if (moveMap[antGo.row][antGo.col] && engine.passable(antGo)) {
					moveMap[antGo.row][antGo.col] = false;
					engine.issueOrder(antLoc, direction);
					blocked = false;
					break;
				}
			}
			if (blocked) {
				moveMap[antLoc.row][antLoc.col] = false;
			}
		}
	}

}

void main() {
	version(unittest) {
		// We don't run the bot and wait for input in this case
	} else {
		Ants.run(new RandomBot());
	}
}