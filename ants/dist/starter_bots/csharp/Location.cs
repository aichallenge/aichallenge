using System;
using System.Collections.Generic;

namespace Ants {

	public class Location {
		/// <summary>
		/// Gets the row of this location.
		/// </summary>
		public int Row { get; private set; }

		/// <summary>
		/// Gets the column of this location.
		/// </summary>
		public int Col { get; private set; }

		public Location (int row, int col) {
			this.Row = row;
			this.Col = col;
		}
		
	}
	
	public class AntLoc : Location {

		/// <summary>
		/// Gets the team of this ant.
		/// </summary>
		public int Team { get; private set; }

		public AntLoc (int row, int col, int team) : base (row, col) {
			this.Team = team;
		}
	}
	
	public class LocationComparer : IEqualityComparer<Location> {
		public bool Equals(Location loc1, Location loc2) {
			return (loc1.Row == loc2.Row && loc1.Col == loc2.Col);
		}
	
		public int GetHashCode(Location loc) {
			return loc.Row * int.MaxValue + loc.Col;
		}
	}
}

