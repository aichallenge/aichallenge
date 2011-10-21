using System;
using System.Collections.Generic;

namespace Ants {

	public class Location : IEquatable<Location> {

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

		public override bool Equals (object obj) {
			if (ReferenceEquals (null, obj))
				return false;
			if (ReferenceEquals (this, obj))
				return true;
			if (obj.GetType() != typeof (Location))
				return false;

			return Equals ((Location) obj);
		}

		public bool Equals (Location other) {
			if (ReferenceEquals (null, other))
				return false;
			if (ReferenceEquals (this, other))
				return true;

			return other.Row == this.Row && other.Col == this.Col;
		}

		public override int GetHashCode()
		{
			unchecked {
				return (this.Row * 397) ^ this.Col;
			}
		}

		public static bool operator == (Location left, Location right) {
			return Equals (left, right);
		}

		public static bool operator != (Location left, Location right) {
			return !Equals (left, right);
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
}

