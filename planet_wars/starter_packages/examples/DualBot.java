import java.util.*;

public class DualBot {
    public static void DoTurn(PlanetWars pw) {
        int numFleets = 1;
	boolean attackMode = false;
	if (pw.NumShips(1) > pw.NumShips(2)) {
	    if (pw.Production(1) > pw.Production(2)) {
		numFleets = 1;
		attackMode = true;
	    } else {
		numFleets = 3;
	    }
	} else {
	    if (pw.Production(1) > pw.Production(2)) {
		numFleets = 1;
	    } else {
		numFleets = 5;
	    }	    
	}
	// (1) If we current have more tha numFleets fleets in flight, just do
	// nothing until at least one of the fleets arrives.
	if (pw.MyFleets().size() >= numFleets) {
	    return;
	}
	// (2) Find my strongest planet.
	Planet source = null;
	double sourceScore = Double.MIN_VALUE;
	for (Planet p : pw.MyPlanets()) {
	    double score = (double)p.NumShips() / (1 + p.GrowthRate());
	    if (score > sourceScore) {
		sourceScore = score;
		source = p;
	    }
	}
	// (3) Find the weakest enemy or neutral planet.
	Planet dest = null;
	double destScore = Double.MIN_VALUE;
	List<Planet> candidates = pw.NotMyPlanets();
	if (attackMode) {
	    candidates = pw.EnemyPlanets();
	}
	for (Planet p : candidates) {
	    double score = (double)(1 + p.GrowthRate()) / p.NumShips();
	    if (score > destScore) {
		destScore = score;
		dest = p;
	    }
	}
	// (4) Send half the ships from my strongest planet to the weakest
	// planet that I do not own.
	if (source != null && dest != null) {
	    int numShips = source.NumShips() / 2;
	    pw.IssueOrder(source, dest, numShips);
	}
    }

    public static void main(String[] args) {
	String line = "";
	String message = "";
	int c;
	try {
	    while ((c = System.in.read()) >= 0) {
		switch (c) {
		case '\n':
		    if (line.equals("go")) {
			PlanetWars pw = new PlanetWars(message);
			DoTurn(pw);
		        pw.FinishTurn();
			message = "";
		    } else {
			message += line + "\n";
		    }
		    line = "";
		    break;
		default:
		    line += (char)c;
		    break;
		}
	    }
	} catch (Exception e) {
	    // Owned.
	}
    }
}

