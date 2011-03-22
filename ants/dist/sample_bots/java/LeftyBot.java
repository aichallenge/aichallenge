import java.util.*;

public class LeftyBot implements Bot {
	public static void main(String[] args) {
		Ants.run(new LeftyBot());
	}

	// things to remember between turns
	private Map<Tile, Aim> antStraight = new HashMap<Tile, Aim>();
	private Map<Tile, Aim> antLefty = new HashMap<Tile, Aim>();

	// bot logic, run once per turn
	public void do_turn(Ants ants) {
		Set<Tile> destinations = new HashSet<Tile>();
		Map<Tile, Aim> newStraight = new HashMap<Tile, Aim>();
		Map<Tile, Aim> newLefty = new HashMap<Tile, Aim>();
		for (Tile location : ants.myAnts()) {
			// send new ants in a straight line
			if (!antStraight.containsKey(location) && !antLefty.containsKey(location)) {
				Aim direction;
				if (location.row() % 2 == 0) {
					if (location.col() % 2 == 0) {
						direction = Aim.NORTH;
					} else {
						direction = Aim.SOUTH;
					}
				} else {
					if (location.col() % 2 == 0) {
						direction = Aim.EAST;
					} else {
						direction = Aim.WEST;
					}
				}
				antStraight.put(location, direction);
			}
			// send ants going in a straight line in the same direction
			if (antStraight.containsKey(location)) {
				Aim direction = antStraight.get(location);
				Tile destination = ants.tile(location, direction);
				if (ants.ilk(destination).isPassable()) {
					if (ants.ilk(destination).isUnoccupied() && !destinations.contains(destination)) {
						ants.issueOrder(location, direction);
						newStraight.put(destination, direction);
						destinations.add(destination);
					} else {
						// pause ant, turn and try again next turn
						newStraight.put(location, direction.left());
						destinations.add(location);
					}
				} else {
					// hit a wall, start following it
					antLefty.put(location, direction.right());
				}
			}
			// send ants following a wall, keeping it on their left
			if (antLefty.containsKey(location)) {
				Aim direction = antLefty.get(location);
				List<Aim> directions = new ArrayList<Aim>();
				directions.add(direction.left());
				directions.add(direction);
				directions.add(direction.right());
				directions.add(direction.behind());
				// try 4 directions in order, attempting to turn left at corners
				for (Aim new_direction : directions) {
					Tile destination = ants.tile(location, new_direction);
					if (ants.ilk(destination).isPassable()) {
						if (ants.ilk(destination).isUnoccupied() && !destinations.contains(destination)) {
							ants.issueOrder(location, new_direction);
							newLefty.put(destination, new_direction);
							destinations.add(destination);
							break;
						} else {
							// pause ant, turn and send straight
							newStraight.put(location, direction.right());
							destinations.add(location);
							break;
						}
					}
				}
			}
		}
		antStraight = newStraight;
		antLefty = newLefty;
	}
}