import java.util.*;

public class RandomBot implements Runnable {
	private static Ants ants = new Ants();
	public static void main(String[] args) {
		Ants.run(new RandomBot(), ants);
	}
	
	public void run() {
		Set<Tile> destinations = new HashSet<Tile>();
		for (Tile location : ants.myAnts()) {
			List<Aim> directions = new ArrayList<Aim>(EnumSet.allOf(Aim.class));
			Collections.shuffle(directions);
			boolean issued = false;
			for (Aim direction : directions) {
				Tile destination = ants.tile(location, direction);
				if (ants.ilk(destination).isUnoccupied() && !destinations.contains(destination)) {
					ants.issueOrder(location, direction);
					destinations.add(destination);
					issued = true;
					break;
				}
			}
			if (!issued) {
				destinations.add(location);
			}
		}
	}
}

