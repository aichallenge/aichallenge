using System;

namespace Ants {
	public abstract class Bot {
				
		public abstract void doTurn(GameState state);
		
		protected void issueOrder(Location loc, char direction) {
			System.Console.Out.WriteLine("o {0} {1} {2}", loc.row, loc.col, direction);
		}
	}
}