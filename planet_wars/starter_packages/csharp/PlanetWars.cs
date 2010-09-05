// Contestants do not need to worry about anything in this file. This is just
// helper code that does the boring stuff for you, so you can focus on the
// interesting stuff. That being said, you're welcome to change anything in
// this file if you know what you're doing.

using System;
using System.IO;
using System.Collections.Generic;

public class PlanetWars {
    // Constructs a PlanetWars object instance, given a string containing a
    // description of a game state.
    public PlanetWars(string gameStatestring) {
	planets = new List<Planet>();
	fleets = new List<Fleet>();
	ParseGameState(gameStatestring);
    }

    // Returns the number of planets. Planets are numbered starting with 0.
    public int NumPlanets() {
	return planets.Count;
    }

    // Returns the planet with the given planet_id. There are NumPlanets()
    // planets. They are numbered starting at 0.
    public Planet GetPlanet(int planetID) {
	return planets[planetID];
    }

    // Returns the number of fleets.
    public int NumFleets() {
	return fleets.Count;
    }

    // Returns the fleet with the given fleet_id. Fleets are numbered starting
    // with 0. There are NumFleets() fleets. fleet_id's are not consistent from
    // one turn to the next.
    public Fleet GetFleet(int fleetID) {
	return fleets[fleetID];
    }

    // Returns a list of all the planets.
    public List<Planet> Planets() {
	return planets;
    }

    // Return a list of all the planets owned by the current player. By
    // convention, the current player is always player number 1.
    public List<Planet> MyPlanets() {
	List<Planet> r = new List<Planet>();
	foreach (Planet p in planets) {
	    if (p.Owner() == 1) {
		r.Add(p);
	    }
	}
	return r;
    }

    // Return a list of all neutral planets.
    public List<Planet> NeutralPlanets() {
	List<Planet> r = new List<Planet>();
	foreach (Planet p in planets) {
	    if (p.Owner() == 0) {
		r.Add(p);
	    }
	}
	return r;
    }

    // Return a list of all the planets owned by rival players. This excludes
    // planets owned by the current player, as well as neutral planets.
    public List<Planet> EnemyPlanets() {
	List<Planet> r = new List<Planet>();
	foreach (Planet p in planets) {
	    if (p.Owner() >= 2) {
		r.Add(p);
	    }
	}
	return r;
    }

    // Return a list of all the planets that are not owned by the current
    // player. This includes all enemy planets and neutral planets.
    public List<Planet> NotMyPlanets() {
	List<Planet> r = new List<Planet>();
	foreach (Planet p in planets) {
	    if (p.Owner() != 1) {
		r.Add(p);
	    }
	}
	return r;
    }

    // Return a list of all the fleets.
    public List<Fleet> Fleets() {
	List<Fleet> r = new List<Fleet>();
	foreach (Fleet f in fleets) {
            r.Add(f);
	}
	return r;
    }

    // Return a list of all the fleets owned by the current player.
    public List<Fleet> MyFleets() {
	List<Fleet> r = new List<Fleet>();
	foreach (Fleet f in fleets) {
	    if (f.Owner() == 1) {
		r.Add(f);
	    }
	}
	return r;
    }

    // Return a list of all the fleets owned by enemy players.
    public List<Fleet> EnemyFleets() {
	List<Fleet> r = new List<Fleet>();
	foreach (Fleet f in fleets) {
	    if (f.Owner() != 1) {
		r.Add(f);
	    }
	}
	return r;
    }

    // Returns the distance between two planets, rounded up to the next highest
    // integer. This is the number of discrete time steps it takes to get
    // between the two planets.
    public int Distance(int sourcePlanet, int destinationPlanet) {
	Planet source = planets[sourcePlanet];
	Planet destination = planets[destinationPlanet];
	double dx = source.X() - destination.X();
	double dy = source.Y() - destination.Y();
	return (int)Math.Ceiling(Math.Sqrt(dx * dx + dy * dy));
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
        Console.WriteLine("" + sourcePlanet + " " + destinationPlanet + " " +
          numShips);
	Console.Out.Flush();
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
    public void IssueOrder(Planet source, Planet dest, int numShips) {
        Console.WriteLine("" + source.PlanetID() + " " + dest.PlanetID() +
          " " + numShips);
	Console.Out.Flush();
    }

    // Sends the game engine a message to let it know that we're done sending
    // orders. This signifies the end of our turn.
    public void FinishTurn() {
	Console.WriteLine("go");
	Console.Out.Flush();
    }

    // Returns true if the named player owns at least one planet or fleet.
    // Otherwise, the player is deemed to be dead and false is returned.
    public bool IsAlive(int playerID) {
	foreach (Planet p in planets) {
	    if (p.Owner() == playerID) {
		return true;
	    }
	}
	foreach (Fleet f in fleets) {
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
	List<int> remainingPlayers = new List<int>();
	foreach (Planet p in planets) {
            if (!remainingPlayers.Contains(p.Owner())) {
	        remainingPlayers.Add(p.Owner());
            }
	}
	foreach (Fleet f in fleets) {
	    if (!remainingPlayers.Contains(f.Owner())) {
                remainingPlayers.Add(f.Owner());
            }
	}
	switch (remainingPlayers.Count) {
	case 0:
	    return 0;
	case 1:
	    return remainingPlayers[0];
	default:
	    return -1;
	}
    }

    // Returns the number of ships that the current player has, either located
    // on planets or in flight.
    public int NumShips(int playerID) {
	int numShips = 0;
	foreach (Planet p in planets) {
	    if (p.Owner() == playerID) {
		numShips += p.NumShips();
	    }
	}
	foreach (Fleet f in fleets) {
	    if (f.Owner() == playerID) {
		numShips += f.NumShips();
	    }
	}
	return numShips;
    }

    // Parses a game state from a string. On success, returns 1. On failure,
    // returns 0.
    private int ParseGameState(string s) {
	planets.Clear();
	fleets.Clear();
	int planetID = 0;
	string[] lines = s.Split('\n');
	for (int i = 0; i < lines.Length; ++i) {
	    string line = lines[i];
	    int commentBegin = line.IndexOf('#');
	    if (commentBegin >= 0) {
		line = line.Substring(0, commentBegin);
	    }
	    if (line.Trim().Length == 0) {
		continue;
	    }
	    string[] tokens = line.Split(' ');
	    if (tokens.Length == 0) {
		continue;
	    }
	    if (tokens[0].Equals("P")) {
		if (tokens.Length != 6) {
		    return 0;
		}
		double x = double.Parse(tokens[1]);
		double y = double.Parse(tokens[2]);
		int owner = int.Parse(tokens[3]);
		int numShips = int.Parse(tokens[4]);
		int growthRate = int.Parse(tokens[5]);
		Planet p = new Planet(planetID++,
				      owner,
				      numShips,
				      growthRate,
				      x, y);
		planets.Add(p);
	    } else if (tokens[0].Equals("F")) {
		if (tokens.Length != 7) {
		    return 0;
		}
		int owner = int.Parse(tokens[1]);
		int numShips = int.Parse(tokens[2]);
		int source = int.Parse(tokens[3]);
		int destination = int.Parse(tokens[4]);
		int totalTripLength = int.Parse(tokens[5]);
		int turnsRemaining = int.Parse(tokens[6]);
		Fleet f = new Fleet(owner,
				    numShips,
				    source,
				    destination,
				    totalTripLength,
				    turnsRemaining);
		fleets.Add(f);
	    } else {
		return 0;
	    }
	}
	return 1;
    }

    // Store all the planets and fleets. OMG we wouldn't wanna lose all the
    // planets and fleets, would we!?
    private List<Planet> planets;
    private List<Fleet> fleets;
}
