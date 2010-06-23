import java.util.*;
import java.lang.Math;
import java.io.*;

public class PlanetWars {
    // There are two modes:
    //   * If mode == 0, then s is interpreted as a filename, and the game is
    //     initialized by reading map data out of the named file.
    //   * If mode == 1, then s is interpreted as a string that contains map
    //     data directly. The string is parsed in the same way that the
    //     contents of a map file would be.
    // This constructor does not actually initialize the game object. You must
    // always call Init() before the game object will be in any kind of
    // coherent state.
    public PlanetWars(String s, int mode) {
	planets = new ArrayList<Planet>();
	fleets = new ArrayList<Fleet>();
	initMode = mode;
	switch (initMode) {
	case 0:
	    mapFilename = s;
	    break;
	case 1:
	    mapData = s;
	    break;
	default:
	    break;
	}
    }

    // Initializes a game of Planet Wars. Loads the map data from the file
    // specified in the constructor. Returns 1 on success, 0 on failure.
    public int Init() {
	switch (initMode) {
	case 0:
	    return LoadMapFromFile(mapFilename);
	case 1:
	    return ParseGameState(mapData);
	default:
	    return 0;
	}
    }

    // Returns the number of planets. Planets are numbered starting with 0.
    public int NumPlanets() {
	return planets.size();
    }

    // Returns the planet with the given planet_id. There are NumPlanets()
    // planets. They are numbered starting at 0.
    public Planet GetPlanet(int planetID) {
	return planets.get(planetID);
    }

    // Returns the number of fleets.
    public int NumFleets() {
	return fleets.size();
    }

    // Returns the fleet with the given fleet_id. Fleets are numbered starting
    // with 0. There are NumFleets() fleets. fleet_id's are not consistent from
    // one turn to the next.
    public Fleet GetFleet(int fleetID) {
	return fleets.get(fleetID);
    }

    // Returns the distance between two planets, rounded up to the next highest
    // integer. This is the number of discrete time steps it takes to get
    // between the two planets.
    public int Distance(int sourcePlanet, int destinationPlanet) {
	Planet source = planets.get(sourcePlanet);
	Planet destination = planets.get(destinationPlanet);
	double dx = source.X() - destination.X();
	double dy = source.Y() - destination.Y();
	return (int)Math.ceil(Math.sqrt(dx * dx + dy * dy));
    }

    // Sends an order to the game engine. An order is composed of a source
    // planet number, a destination planet number, and a number of ships. A
    // few things to keep in mind:
    //   * you can issue many orders per turn if you like.
    //   * the planets are numbered starting at zero, not one.
    //   * you must own the source planet. If you break this rule, the game
    //     engine kicks your bot out of the game instantly.
    //   * you can't move more ships than are currently on the source planet.
    //   * the ships will take a few turns to reach their destination. Travel
    //     is not instant. See the Distance() function for more info.
    public void IssueOrder(int sourcePlanet,
                          int destinationPlanet,
                          int numShips) {
        System.out.println("" + sourcePlanet + " " + destinationPlanet + " " +
          numShips);
    }

    // Returns true if the named player owns at least one planet or fleet.
    // Otherwise, the player is deemed to be dead and false is returned.
    public boolean IsAlive(int playerID) {
	for (Planet p : planets) {
	    if (p.Owner() == playerID) {
		return true;
	    }
	}
	for (Fleet f : fleets) {
	    if (f.Owner() == playerID) {
		return true;
	    }
	}
	return false;
    }

    // If the game is not yet over (ie: at least two players have planets or
    // fleets remaining), returns -1. If the game is over (ie: only one player
    // is left) then that player's number is returned. If there are no
    // remaining players, then the game is a draw and 0 is returned.
    public int Winner() {
	Set<Integer> remainingPlayers = new TreeSet<Integer>();
	for (Planet p : planets) {
	    remainingPlayers.add(p.Owner());
	}
	for (Fleet f : fleets) {
	    remainingPlayers.add(f.Owner());
	}
	switch (remainingPlayers.size()) {
	case 0:
	    return 0;
	case 1:
	    return ((Integer)remainingPlayers.toArray()[0]).intValue();
	default:
	    return -1;
	}
    }

    // Returns the number of ships that the current player has, either located
    // on planets or in flight.
    public int NumShips(int playerID) {
	int numShips = 0;
	for (Planet p : planets) {
	    if (p.Owner() == playerID) {
		numShips += p.NumShips();
	    }
	}
	for (Fleet f : fleets) {
	    if (f.Owner() == playerID) {
		numShips += f.NumShips();
	    }
	}
	return numShips;
    }

    // Parses a game state from a string. On success, returns 1. On failure,
    // returns 0.
    private int ParseGameState(String s) {
	planets.clear();
	fleets.clear();
	String[] lines = s.split("\n");
	for (int i = 0; i < lines.length; ++i) {
	    String line = lines[i];
	    int commentBegin = line.indexOf('#');
	    if (commentBegin >= 0) {
		line = line.substring(0, commentBegin);
	    }
	    if (line.trim().length() == 0) {
		continue;
	    }
	    String[] tokens = line.split(" ");
	    if (tokens.length == 0) {
		continue;
	    }
	    if (tokens[0].equals("P")) {
		if (tokens.length != 6) {
		    return 0;
		}
		double x = Double.parseDouble(tokens[1]);
		double y = Double.parseDouble(tokens[2]);
		int owner = Integer.parseInt(tokens[3]);
		int numShips = Integer.parseInt(tokens[4]);
		int growthRate = Integer.parseInt(tokens[5]);
		Planet p = new Planet(owner, numShips, growthRate, x, y);
		planets.add(p);
	    } else if (tokens[0].equals("F")) {
		if (tokens.length != 7) {
		    return 0;
		}
		int owner = Integer.parseInt(tokens[1]);
		int numShips = Integer.parseInt(tokens[2]);
		int source = Integer.parseInt(tokens[3]);
		int destination = Integer.parseInt(tokens[4]);
		int totalTripLength = Integer.parseInt(tokens[5]);
		int turnsRemaining = Integer.parseInt(tokens[6]);
		Fleet f = new Fleet(owner,
				    numShips,
				    source,
				    destination,
				    totalTripLength,
				    turnsRemaining);
		fleets.add(f);
	    } else {
		return 0;
	    }
	}
	return 1;
    }

    // Loads a map from a test file. The text file contains a description of
    // the starting state of a game. See the project wiki for a description of
    // the file format. It should be called the Planet Wars Point-in-Time
    // format. On success, return 1. On failure, returns 0.
    private int LoadMapFromFile(String mapFilename) {
	String s = "";
	BufferedReader in = null;
	try {
		in = new BufferedReader(new FileReader(mapFilename));
		int c;
		while ((c = in.read()) >= 0) {
		    s += (char)c;
		}
	} catch (Exception e) {
	    return 0;
	} finally {
	    try {
		in.close();
	    } catch (Exception e) {
		// Fucked.
	    }
	}
	return ParseGameState(s);
    }

    // Store all the planets and fleets. OMG we wouldn't wanna lose all the
    // planets and fleets, would we!?
    private ArrayList<Planet> planets;
    private ArrayList<Fleet> fleets;

    // The filename of the map that this game is being played on. This would
    // usually be null for the purposes of contestants.
    private String mapFilename;

    // The string of map data to parse.
    private String mapData;

    // Stores a mode identifier which determines how to initialize this object.
    // See the constructor for details.
    private int initMode;
}

