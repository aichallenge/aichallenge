using System;

namespace Ants {

	public enum Direction {
		North,
		South,
		East,
		West
	}

	public static class DirectionExtensions {

		public static char ToChar (this Direction self) {
			switch (self)
			{
				case Direction.East:
					return 'e';

				case Direction.North:
					return 'n';

				case Direction.South:
					return 's';

				case Direction.West:
					return 'w';

				default:
					throw new ArgumentException ("Unknown direction", "self");
			}
		}
	}
}