import java.util.*;

public class RandomBot {
    public static void DoTurn(PlanetWars pw) {
	// (1) If we current have a fleet in flight, then do nothing until it
	// arrives.
	if (pw.MyFleets().size() >= 1) {
	    return;
	}
	// (2) Pick one of my planets at random.
	Random r = new Random();
	Planet source = null;
	List<Planet> p = pw.MyPlanets();
	if (p.size() > 0) {
	    source = p.get(r.nextInt(p.size()));
	}
	// (3) Pick a target planet at random.
	Planet dest = null;
	p = pw.Planets();
	if (p.size() > 0) {
	    dest = p.get(r.nextInt(p.size()));
	}
	// (4) Send half the ships from source to dest.
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

