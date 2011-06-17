using System;
using System.Collections.Generic;
using System.Linq;

namespace Ants {

	class GreedyBot : Bot {
		
		Random rng;
		
		HashSet<Location> destinations;
		
		public GreedyBot () {
			rng = new Random(42);
			destinations = new HashSet<Location>(new LocationComparer());
		}
				
		// doTurn is run once per turn
		public override void doTurn (GameState state) {
			
			destinations.Clear();
			
			// loop through all my ants and try to give them orders
			foreach (AntLoc ant in state.MyAnts) {
				
				Location closestFood = null;
				int closestDist = int.MaxValue;
				foreach (Location food in state.FoodTiles) {
					int dist = state.distance(ant, food);
					if (dist < closestDist) {
						closestDist = dist;
						closestFood = food;
					}
				}
				
				IEnumerable<char> directions;
				if (closestFood != null) directions = state.direction(ant, closestFood);
				else directions = Ants.Aim.Keys.Shuffle(rng);
				
				// try all the directions
				foreach (char direction in directions) {
					
					// destination will wrap around the map properly
					// and give us a new location
					Location newLoc = state.destination(ant, direction);
					
					// passable returns true if the location is land
					if (state.unoccupied(newLoc) && !destinations.Contains(newLoc)) {
						issueOrder(ant, direction);
						destinations.Add(newLoc);
						// stop now, don't give 1 and multiple orders
						break;
					}
				}
				
				// check if we have time left to calculate more orders
				if (state.TimeRemaining < 10) break;
			}
			
		}
		
		public static void Main (string[] args) {
			new Ants().playGame(new GreedyBot());
		}

	}
	
	public static class Extensions {
		// Fisher-Yates shuffle courtesy of StackOverflow
		public static IEnumerable<T> Shuffle<T>(this IEnumerable<T> source, Random rng) {
			T[] elements = source.ToArray();
			// Note i > 0 to avoid final pointless iteration
			for (int i = elements.Length-1; i > 0; i--) {
				// Swap element "i" with a random earlier element it (or itself)
				int swapIndex = rng.Next(i + 1);
				T tmp = elements[i];
				elements[i] = elements[swapIndex];
				elements[swapIndex] = tmp;
			}
			// Lazily yield (avoiding aliasing issues etc)
			foreach (T element in elements) {
				yield return element;
			}
		}	
	}
	
}