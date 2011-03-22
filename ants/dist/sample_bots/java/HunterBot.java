import java.util.*;

public class HunterBot implements Bot {
	public static void main(String[] args) {
		Ants.run(new HunterBot());
	}

	public void do_turn(Ants ants) {
		Set<Tile> destinations = new HashSet<Tile>();
		Set<Tile> targets = new HashSet<Tile>();
		targets.addAll(ants.food());
		targets.addAll(ants.enemyAnts());
		for (Tile location : ants.myAnts()) {
			boolean issued = false;
			Tile closestTarget = null;
			int closestDistance = 999999;
			for (Tile target : targets) {
				int distance = ants.distance(location, target);
				if (distance < closestDistance) {
					closestDistance = distance;
					closestTarget = target;
				}
			}
			if (closestTarget != null) {
				List<Aim> directions = ants.directions(location, closestTarget);
				Collections.shuffle(directions);
				for (Aim direction : directions) {
					Tile destination = ants.tile(location, direction);
					if (ants.ilk(destination).isUnoccupied() && !destinations.contains(destination)) {
						ants.issueOrder(location, direction);
						destinations.add(destination);
						issued = true;
						break;
					}
				}
			}
			if (!issued) {
				destinations.add(location);
			}
		}
	}
}

