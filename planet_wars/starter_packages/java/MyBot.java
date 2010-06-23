import java.util.*;

public class MyBot {
    public static void DoTurn(PlanetWars pw, Random r) {
	for (int i = 0; i < pw.NumFleets(); ++i) {
	    if (pw.GetFleet(i).Owner() == 1) {
		System.out.println("go");
		return;
	    }
	}
	List<Integer> myPlanets = new ArrayList<Integer>();
	List<Integer> opPlanets = new ArrayList<Integer>();
	for (int i = 0; i < pw.NumPlanets(); ++i) {
	    Planet p = pw.GetPlanet(i);
	    if (p.Owner() == 1) {
		myPlanets.add(i);
	    } else {
		opPlanets.add(i);
	    }
	}
	if (myPlanets.size() > 0 && opPlanets.size() > 0) {
	    int source = myPlanets.get(r.nextInt(myPlanets.size()));
	    int destination = opPlanets.get(r.nextInt(opPlanets.size()));
	    int numShips = r.nextInt(pw.GetPlanet(source).NumShips());
	    System.out.println("" + source + " " +
			       destination + " " + numShips);
	}
	System.out.println("go");
    }

    public static void main(String[] args) {
	String line = "";
	String message = "";
	int c;
	try {
	    Random r = new Random();
	    while ((c = System.in.read()) >= 0) {
		switch (c) {
		case '\n':
		    if (line.equals("go")) {
			PlanetWars pw = new PlanetWars(message, 1);
			if (pw.Init() == 1) {
			    DoTurn(pw, r);
			}
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

