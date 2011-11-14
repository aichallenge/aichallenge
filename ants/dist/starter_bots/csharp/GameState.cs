using System;
using System.Collections.Generic;

namespace Ants {
	
	public class GameState : IGameState {
		
		public int Width { get; private set; }
		public int Height { get; private set; }
		
		public int LoadTime { get; private set; }
		public int TurnTime { get; private set; }
		
		private DateTime turnStart;
		public int TimeRemaining {
			get {
				TimeSpan timeSpent = DateTime.Now - turnStart;
				return TurnTime - (int)timeSpent.TotalMilliseconds;
			}
		}

		public int ViewRadius2 { get; private set; }
		public int AttackRadius2 { get; private set; }
		public int SpawnRadius2 { get; private set; }

		public List<Ant> MyAnts { get; private set; }
		public List<AntHill> MyHills { get; private set; }
		public List<Ant> EnemyAnts { get; private set; }
		public List<AntHill> EnemyHills { get; private set; }
		public List<Location> DeadTiles { get; private set; }
		public List<Location> FoodTiles { get; private set; }

		public Tile this[Location location] {
			get { return this.map[location.Row, location.Col]; }
		}

		public Tile this[int row, int col] {
			get { return this.map[row, col]; }
		}
		
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
			
			MyAnts = new List<Ant>();
			MyHills = new List<AntHill>();
			EnemyAnts = new List<Ant>();
			EnemyHills = new List<AntHill>();
			DeadTiles = new List<Location>();
			FoodTiles = new List<Location>();
			
			map = new Tile[height, width];
			for (int row = 0; row < height; row++) {
				for (int col = 0; col < width; col++) {
					map[row, col] = Tile.Land;
				}
			}
		}

		#region State mutators
		public void StartNewTurn () {
			// start timer
			turnStart = DateTime.Now;
			
			// clear ant data
			foreach (Location loc in MyAnts) map[loc.Row, loc.Col] = Tile.Land;
			foreach (Location loc in MyHills) map[loc.Row, loc.Col] = Tile.Land;
			foreach (Location loc in EnemyAnts) map[loc.Row, loc.Col] = Tile.Land;
			foreach (Location loc in EnemyHills) map[loc.Row, loc.Col] = Tile.Land;
			foreach (Location loc in DeadTiles) map[loc.Row, loc.Col] = Tile.Land;
			
			MyHills.Clear();
			MyAnts.Clear();
			EnemyHills.Clear();
			EnemyAnts.Clear();
			DeadTiles.Clear();
			
			// set all known food to unseen
			foreach (Location loc in FoodTiles) map[loc.Row, loc.Col] = Tile.Land;
			FoodTiles.Clear();
		}

		public void AddAnt (int row, int col, int team) {
			map[row, col] = Tile.Ant;
			
			Ant ant = new Ant(row, col, team);
			if (team == 0) {
				MyAnts.Add(ant);
			} else {
				EnemyAnts.Add(ant);
			}
		}

		public void AddFood (int row, int col) {
			map[row, col] = Tile.Food;
			FoodTiles.Add(new Location(row, col));
		}

		public void RemoveFood (int row, int col) {
			// an ant could move into a spot where a food just was
			// don't overwrite the space unless it is food
			if (map[row, col] == Tile.Food) {
				map[row, col] = Tile.Land;
			}
			FoodTiles.Remove(new Location(row, col));
		}

		public void AddWater (int row, int col) {
			map[row, col] = Tile.Water;
		}

		public void DeadAnt (int row, int col) {
			// food could spawn on a spot where an ant just died
			// don't overwrite the space unless it is land
			if (map[row, col] == Tile.Land) {
				map[row, col] = Tile.Dead;
			}
			
			// but always add to the dead list
			DeadTiles.Add(new Location(row, col));
		}

		public void AntHill (int row, int col, int team) {

			if (map[row, col] == Tile.Land) {
				map[row, col] = Tile.Hill;
			}

			AntHill hill = new AntHill (row, col, team);
			if (team == 0)
				MyHills.Add (hill);
			else
				EnemyHills.Add (hill);
		}
		#endregion

		/// <summary>
		/// Gets whether <paramref name="location"/> is passable or not.
		/// </summary>
		/// <param name="location">The location to check.</param>
		/// <returns><c>true</c> if the location is not water, <c>false</c> otherwise.</returns>
		/// <seealso cref="GetIsUnoccupied"/>
		public bool GetIsPassable (Location location) {
			return map[location.Row, location.Col] != Tile.Water;
		}
		
		/// <summary>
		/// Gets whether <paramref name="location"/> is occupied or not.
		/// </summary>
		/// <param name="location">The location to check.</param>
		/// <returns><c>true</c> if the location is passable and does not contain an ant, <c>false</c> otherwise.</returns>
		public bool GetIsUnoccupied (Location location) {
			return GetIsPassable(location) && map[location.Row, location.Col] != Tile.Ant;
		}
		
		/// <summary>
		/// Gets the destination if an ant at <paramref name="location"/> goes in <paramref name="direction"/>, accounting for wrap around.
		/// </summary>
		/// <param name="location">The starting location.</param>
		/// <param name="direction">The direction to move.</param>
		/// <returns>The new location, accounting for wrap around.</returns>
		public Location GetDestination (Location location, Direction direction) {
			Location delta = Ants.Aim[direction];
			
			int row = (location.Row + delta.Row) % Height;
			if (row < 0) row += Height; // because the modulo of a negative number is negative

			int col = (location.Col + delta.Col) % Width;
			if (col < 0) col += Width;
			
			return new Location(row, col);
		}

		/// <summary>
		/// Gets the distance between <paramref name="loc1"/> and <paramref name="loc2"/>.
		/// </summary>
		/// <param name="loc1">The first location to measure with.</param>
		/// <param name="loc2">The second location to measure with.</param>
		/// <returns>The distance between <paramref name="loc1"/> and <paramref name="loc2"/></returns>
		public int GetDistance (Location loc1, Location loc2) {
			int d_row = Math.Abs(loc1.Row - loc2.Row);
			d_row = Math.Min(d_row, Height - d_row);
			
			int d_col = Math.Abs(loc1.Col - loc2.Col);
			d_col = Math.Min(d_col, Width - d_col);
			
			return d_row + d_col;
		}

		/// <summary>
		/// Gets the closest directions to get from <paramref name="loc1"/> to <paramref name="loc2"/>.
		/// </summary>
		/// <param name="loc1">The location to start from.</param>
		/// <param name="loc2">The location to determine directions towards.</param>
		/// <returns>The 1 or 2 closest directions from <paramref name="loc1"/> to <paramref name="loc2"/></returns>
		public ICollection<Direction> GetDirections (Location loc1, Location loc2) {
			List<Direction> directions = new List<Direction>();
			
			if (loc1.Row < loc2.Row) {
				if (loc2.Row - loc1.Row >= Height / 2)
					directions.Add(Direction.North);
				if (loc2.Row - loc1.Row <= Height / 2)
					directions.Add(Direction.South);
			}
			if (loc2.Row < loc1.Row) {
				if (loc1.Row - loc2.Row >= Height / 2)
					directions.Add(Direction.South);
				if (loc1.Row - loc2.Row <= Height / 2)
					directions.Add(Direction.North);
			}
			
			if (loc1.Col < loc2.Col) {
				if (loc2.Col - loc1.Col >= Width / 2)
					directions.Add(Direction.West);
				if (loc2.Col - loc1.Col <= Width / 2)
					directions.Add(Direction.East);
			}
			if (loc2.Col < loc1.Col) {
				if (loc1.Col - loc2.Col >= Width / 2)
					directions.Add(Direction.East);
				if (loc1.Col - loc2.Col <= Width / 2)
					directions.Add(Direction.West);
			}
			
			return directions;
		}
		
		public bool GetIsVisible(Location loc)
		{
			List<Location> offsets = new List<Location>();
			int squares = (int)Math.Floor(Math.Sqrt(this.ViewRadius2));
			for (int r = -1 * squares; r <= squares; ++r)
			{
				for (int c = -1 * squares; c <= squares; ++c)
				{
					int square = r * r + c * c;
					if (square < this.ViewRadius2)
					{
						offsets.Add(new Location(r, c));
					}
				}
			}
			foreach (Ant ant in this.MyAnts)
			{
				foreach (Location offset in offsets)
				{
					if ((ant.Col + offset.Col) == loc.Col &&
						(ant.Row + offset.Row) == loc.Row)
					{
								 return true;
					}
				}
			}
			return false;
		}

	}
}

