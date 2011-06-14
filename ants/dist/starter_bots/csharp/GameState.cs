using System;
using System.Collections.Generic;

namespace Ants {
	
	public class GameState {
		
		public int Width { get; private set; }
		public int Height { get; private set; }
		
		public int LoadTime { get; private set; }
		public int TurnTime { get; private set; }
		
		private DateTime turnStart;
		public int TimeRemaining {
			get {
				TimeSpan timeSpent = DateTime.Now - turnStart;
				return TurnTime - timeSpent.Milliseconds;
			}
		}

		public int ViewRadius2 { get; private set; }
		public int AttackRadius2 { get; private set; }
		public int SpawnRadius2 { get; private set; }
		
		public List<AntLoc> MyAnts;
		public List<AntLoc> EnemyAnts;
		public List<Location> DeadTiles;
		public List<Location> FoodTiles;
		
		private Tile[,] map;
		
		public GameState (int width, int height, 
		                  int turntime, int loadtime, 
		                  int viewradius2, int attackradius2, int spawnradius2) {
			
			Width = width;
			Height = height;
			
			LoadTime = loadtime;
			TurnTime = turntime;
			
			ViewRadius2 = viewradius2;
			AttackRadius2 = attackradius2;
			SpawnRadius2 = spawnradius2;
			
			MyAnts = new List<AntLoc>();
			EnemyAnts = new List<AntLoc>();
			DeadTiles = new List<Location>();
			FoodTiles = new List<Location>();
			
			map = new Tile[height, width];
			for (int row = 0; row < height; row++) {
				for (int col = 0; col < width; col++) {
					map[row, col] = Tile.Land;
				}
			}
		}
		
		
		public void startNewTurn () {
			// start timer
			turnStart = DateTime.Now;
			
			// clear ant data
			foreach (Location loc in MyAnts) map[loc.row, loc.col] = Tile.Land;
			foreach (Location loc in EnemyAnts) map[loc.row, loc.col] = Tile.Land;
			foreach (Location loc in DeadTiles) map[loc.row, loc.col] = Tile.Land;
			
			MyAnts.Clear();
			EnemyAnts.Clear();
			DeadTiles.Clear();
			
			// set all known food to unseen
			foreach (Location loc in FoodTiles) map[loc.row, loc.col] = Tile.Land;
			FoodTiles.Clear();
		}
		
		public void addAnt (int row, int col, int team) {
			map[row, col] = Tile.Ant;
			
			AntLoc ant = new AntLoc(row, col, team);
			if (team == 0) {
				MyAnts.Add(ant);
			} else {
				EnemyAnts.Add(ant);
			}
		}
		
		public void addFood (int row, int col) {
			map[row, col] = Tile.Food;
			FoodTiles.Add(new Location(row, col));
		}
		
		public void removeFood (int row, int col) {
			// an ant could move into a spot where a food just was
			// don't overwrite the space unless it is food
			if (map[row, col] == Tile.Food) {
				map[row, col] = Tile.Land;
			}
			FoodTiles.Remove(new Location(row, col));
		}
		
		public void addWater (int row, int col) {
			map[row, col] = Tile.Water;
		}
		
		public void deadAnt (int row, int col) {
			// food could spawn on a spot where an ant just died
			// don't overwrite the space unless it is land
			if (map[row, col] == Tile.Land) {
				map[row, col] = Tile.Dead;
			}
			
			// but always add to the dead list
			DeadTiles.Add(new Location(row, col));
		}
		
		
		
		public bool passable (Location loc) {
			// true if not water
			return map[loc.row, loc.col] != Tile.Water;
		}
		
		public bool unoccupied (Location loc) {
			// true if no ants are at the location
			return passable(loc) && map[loc.row, loc.col] != Tile.Ant;
		}
		
		public Location destination (Location loc, char direction) {
			// calculate a new location given the direction and wrap correctly
			Location delta = Ants.Aim[direction];
			
			int row = (loc.row + delta.row) % Height;
			if (row < 0) row += Height; // because the modulo of a negative number is negative
			
			int col = (loc.col + delta.col) % Width;
			if (col < 0) col += Width;
			
			return new Location(row, col);
		}
		
		public int distance (Location loc1, Location loc2) {
			// calculate the closest distance between two locations
			int d_row = Math.Abs(loc1.row - loc2.row);
			d_row = Math.Min(d_row, Height - d_row);
			
			int d_col = Math.Abs(loc1.col - loc2.col);
			d_col = Math.Min(d_col, Width - d_col);
			
			return d_row + d_col;
		}
		
		public ICollection<char> direction (Location loc1, Location loc2) {
			// determine the 1 or 2 fastest (closest) directions to reach a location
			List<char> directions = new List<char>();
			
			if (loc1.row < loc2.row) {
				if (loc2.row - loc1.row >= Height / 2)
					directions.Add('n');
				if (loc2.row - loc1.row <= Height / 2)
					directions.Add('s');
			}
			if (loc2.row < loc1.row) {
				if (loc1.row - loc2.row >= Height / 2)
					directions.Add('s');
				if (loc1.row - loc2.row <= Height / 2)
					directions.Add('n');
			}
			
			if (loc1.col < loc2.col) {
				if (loc2.col - loc1.col >= Width / 2)
					directions.Add('w');
				if (loc2.col - loc1.col <= Width / 2)
					directions.Add('e');
			}
			if (loc2.col < loc1.col) {
				if (loc1.col - loc2.col >= Width / 2)
					directions.Add('e');
				if (loc1.col - loc2.col <= Width / 2)
					directions.Add('w');
			}
			
			return directions;
		}

	}
}

