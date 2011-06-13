using System;
using System.Collections.Generic;

namespace Ants {

	class MyBot : Bot {
				
		// doTurn is run once per turn
		public override void doTurn (GameState state) {
			
			// loop through all my ants and try to give them orders
			foreach (AntLoc ant in state.MyAnts) {
				
				// try all the directions
				foreach (char direction in Ants.Aim.Keys) {
					
					// destination will wrap around the map properly
					// and give us a new location
					Location newLoc = state.destination(ant, direction);
					
					// passable returns true if the location is land
					if (state.passable(newLoc)) {
						issueOrder(ant, direction);
						// stop now, don't give 1 and multiple orders
						break;
					}
				}
				
				// check if we have time left to calculate more orders
				if (state.TimeRemaining < 10) break;
			}
			
		}
		
		
		public static void Main (string[] args) {
			new Ants().playGame(new MyBot());
		}

	}
	
}