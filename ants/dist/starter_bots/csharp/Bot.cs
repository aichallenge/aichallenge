using System;

namespace Ants {
	public abstract class Bot {

		public abstract void DoTurn(GameState state);

		protected void IssueOrder(Location loc, Direction direction) {
			System.Console.Out.WriteLine("o {0} {1} {2}", loc.Row, loc.Col, direction.ToChar());
		}
	}
}