// created using DMD 2.054 64-bit
module MyBot;

import ants;
import std.stdio : stdout;

class MyBot : IBot {

	void doTurn(Ants engine) {
		foreach (antLoc; engine.myAnts) {
			// try all directions until one leads to land
			foreach(direction; AIM) {
				auto antGoto = engine.destination(antLoc, direction);
				if (engine.passable(antGoto)) {
					engine.issueOrder(antLoc, direction);
					break;
				}
			}
		}
	}

}

void main() {
	version(unittest) {
		// We don't run the bot or wait for input in this case
	} else {
		Ants.run(new MyBot());
	}
}
