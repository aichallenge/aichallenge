// Copyright 2010 owners of the AI Challenge project
//
// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy
// of the License at http://www.apache.org/licenses/LICENSE-2.0 . Unless
// required by applicable law or agreed to in writing, software distributed
// under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.
//
// Author:	Jeff Cameron (jeff@jpcameron.com)
//
// Stores the game state.

import java.awt.*;
import java.awt.image.*;
import java.awt.geom.AffineTransform;
import java.util.*;
import java.lang.Math;
import java.io.*;

public class Game implements Cloneable {
    // There are two modes:
    //   * If mode == 0, then s is interpreted as a filename, and the game is
    //     initialized by reading map data out of the named file.
    //   * If mode == 1, then s is interpreted as a string that contains map
    //     data directly. The string is parsed in the same way that the
    //     contents of a map file would be.
    // This constructor does not actually initialize the game object. You must
    // always call Init() before the game object will be in any kind of
    // coherent state.
    public Game(String s, int maxGameLength, int mode, String logFilename) {
	this.logFilename = logFilename;
  planets = new ArrayList<Planet>();
  fleets = new ArrayList<Fleet>();
  gamePlayback = "";
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
  this.maxGameLength = maxGameLength;
  numTurns = 0;
    }

    // Initializes a game of Planet Wars. Loads the map data from the file
    // specified in the constructor. Returns 1 on success, 0 on failure.
    public int Init() {
	// Delete the contents of the log file.
	if (logFilename != null) {
	    try {
		FileOutputStream fos = new FileOutputStream(logFilename);
		fos.close();
		WriteLogMessage("initializing");
	    } catch (Exception e) {
		// do nothing.
	    }
	}

  switch (initMode) {
  case 0:
      return LoadMapFromFile(mapFilename);
  case 1:
      return ParseGameState(mapData);
  default:
      return 0;
  }
    }

    public void WriteLogMessage(String message) {
	if (logFilename == null) {
	    return;
	}
	BufferedWriter bw = null;
	try {
	    bw = new BufferedWriter(new FileWriter(logFilename, true));
	    bw.write(message);
	    bw.newLine();
	    bw.flush();
	} catch (Exception e) {
	    // do nothing.
	} finally {
	    try { bw.close(); } catch (Exception e) { }
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

    // Writes a string which represents the current game state. No point-of-
    // view switching is performed.
    public String toString() {
  return PovRepresentation(-1);
    }

    // Writes a string which represents the current game state. This string
    // conforms to the Point-in-Time format from the project Wiki.
    //
    // Optionally, you may specify the pov (Point of View) parameter. The pov
    // parameter is a player number. If specified, the player numbers 1 and pov
    // will be swapped in the game state output. This is used when sending the
    // game state to individual players, so that they can always assume that
    // they are player number 1.
    public String PovRepresentation(int pov) {
  String s = "";
  for (Planet p : planets) {
      s += "P " + p.X() + " " + p.Y() + " " + PovSwitch(pov, p.Owner()) +
    " " + p.NumShips() + " " + p.GrowthRate() + "\n";
  }
  for (Fleet f : fleets) {
      s += "F " + PovSwitch(pov, f.Owner()) + " " + f.NumShips() + " " +
    f.SourcePlanet() + " " + f.DestinationPlanet() + " " +
    f.TotalTripLength() + " " + f.TurnsRemaining() + "\n";
  }
  return s;
    }

    // Carries out the point-of-view switch operation, so that each player can
    // always assume that he is player number 1. There are three cases.
    // 1. If pov < 0 then no pov switching is being used. Return player_id.
    // 2. If player_id == pov then return 1 so that each player thinks he is
    //    player number 1.
    // 3. If player_id == 1 then return pov so that the real player 1 looks
    //    like he is player number "pov".
    // 4. Otherwise return player_id, since players other than 1 and pov are
    //    unaffected by the pov switch.
    public static int PovSwitch(int pov, int playerID) {
  if (pov < 0) return playerID;
  if (playerID == pov) return 1;
  if (playerID == 1) return pov;
  return playerID;
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

    // Executes one time step.
    //   * Planet bonuses are added to non-neutral planets.
    //   * Fleets are advanced towards their destinations.
    //   * Fleets that arrive at their destination are dealt with.
    public void DoTimeStep() {
  // Add ships to each non-neutral planet according to its growth rate.
  for (Planet p : planets) {
      if (p.Owner() > 0) {
    p.AddShips(p.GrowthRate());
      }
  }
  // Advance all fleets by one time step. Collect the ones that are
  // arriving at their destination planets this turn. Group them by
  // destination and attacking player using the attackers map. For
  // example, attackers[3][4] will store how many of player 4's ships
  // are landing on planet 3 this turn.
  ArrayList<Fleet> newFleets = new ArrayList<Fleet>();
  Map<Integer, Map<Integer, Integer>> attackers =
      new TreeMap<Integer, Map<Integer, Integer>>();
  for (Fleet f : fleets) {
      f.TimeStep();
      if (f.TurnsRemaining() == 0) {
    int dest = f.DestinationPlanet();
    int attacker = f.Owner();
    if (!attackers.containsKey(dest)) {
        attackers.put(dest, new TreeMap<Integer, Integer>());
    }
    if (!attackers.get(dest).containsKey(attacker)) {
        attackers.get(dest).put(attacker, 0);
    }
    int existingAttackers = attackers.get(dest).get(attacker);
    attackers.get(dest).put(attacker,
          existingAttackers + f.NumShips());
      } else {
    newFleets.add(f);
      }
  }
  fleets = newFleets;
  // Resolve the status of each planet which is being attacked. This is
  // non-trivial, since a planet can be attacked by many different
  // players at once.
  for (int i = 0; i < planets.size(); ++i) {
      if (!attackers.containsKey(i)) {
    continue;
      }
      Planet p = planets.get(i);
      int defender = p.Owner();
      // Add the current owner's "attacking" ships to the defending
      // forces.
      if (attackers.get(i).containsKey(defender)) {
    p.AddShips(attackers.get(i).get(defender));
    attackers.get(i).remove(defender);
      }
      // Empty the attackers into a vector of fleets and sort them from
      // weakest to strongest.
      ArrayList<Fleet> enemyFleets = new ArrayList<Fleet>();
      int numEnemyShips = 0;
      for (Integer j : attackers.get(i).keySet()) {
    int numAttackers = attackers.get(i).get(j);
    enemyFleets.add(new Fleet(j, numAttackers));
    numEnemyShips += numAttackers;
      }
      Collections.sort(enemyFleets);
      // Starting with the weakest attacker, the attackers take turns
      // chipping away the defending forces one by one until there are
      // either no more defenders or no more attackers.
      int whoseTurn = 0;
      while (p.NumShips() > 0 && numEnemyShips > 0) {
    if (enemyFleets.get(whoseTurn).NumShips() > 0) {
        p.RemoveShips(1);
        enemyFleets.get(whoseTurn).RemoveShips(1);
        --numEnemyShips;
        if (enemyFleets.get(whoseTurn).NumShips() == 0) {
      enemyFleets.remove(whoseTurn);
      --whoseTurn;
        }
    }
    ++whoseTurn;
    if (whoseTurn >= enemyFleets.size()) {
        whoseTurn = 0;
    }
      }
      // If there are no enemy fleets left, then the defender keeps
      // control of the planet. If there are any enemy fleets left, then
      // they battle it out to determine who gets control of the planet.
      // This is done by cycling through the attackers and subtracting
      // one ship at a time from each, until there is only one attacker
      // left. If the last attackers are all eliminated at the same time,
      // then the planet becomes neutral with zero ships occupying it.
      if (numEnemyShips > 0) {
    p.Owner(0);
    while (true) {
        for (int j = 0; j < enemyFleets.size(); ++j) {
      enemyFleets.get(j).RemoveShips(1);
      if (enemyFleets.get(j).NumShips() <= 0) {
          enemyFleets.remove(j);
          --j;
      }
        }
        if (enemyFleets.size() == 0) {
      break;
        }
        if (enemyFleets.size() == 1) {
      p.Owner(enemyFleets.get(0).Owner());
      p.NumShips(enemyFleets.get(0).NumShips());
      break;
        }
    }
      }
  }
  gamePlayback += ":";
  // Check to see if the maximum number of turns has been reached.
  ++numTurns;
  //System.out.println("Finished turn " + numTurns);
    }

    // Issue an order. This function takes num_ships off the source_planet,
    // puts them into a newly-created fleet, calculates the distance to the
    // destination_planet, and sets the fleet's total trip time to that
    // distance. Checks that the given player_id is allowed to give the given
    // order. If not, the offending player is kicked from the game. If the
    // order was carried out without any issue, and everything is peachy, then
    // 0 is returned. Otherwise, -1 is returned.
    public int IssueOrder(int playerID,
        int sourcePlanet,
        int destinationPlanet,
        int numShips) {
  Planet source = planets.get(sourcePlanet);
  if (source.Owner() != playerID || numShips > source.NumShips()) {
      WriteLogMessage("Dropping player " + playerID + ". source.Owner() = " + source.Owner() + ", playerID = " + playerID + ", numShips = " + numShips + ", source.NumShips() = " + source.NumShips());
      DropPlayer(playerID);
      return -1;
  }
  source.RemoveShips(numShips);
  int distance = Distance(sourcePlanet, destinationPlanet);
  Fleet f = new Fleet(source.Owner(),
          numShips,
          sourcePlanet,
          destinationPlanet,
          distance,
          distance);
  fleets.add(f);
  char lastChar = gamePlayback.charAt(gamePlayback.length() - 1);
  if (lastChar != ':' && lastChar != '|') {
      gamePlayback += ",";
  }
  gamePlayback += "" + sourcePlanet + "." + destinationPlanet + "." +
      numShips;
  return 0;
    }

    // Behaves just like the longer form of IssueOrder, but takes a string
    // of the form "source_planet destination_planet num_ships". That is, three
    // integers separated by space characters.
    public int IssueOrder(int playerID, String order) {
  String[] tokens = order.split(" ");
  if (tokens.length != 3) {
      return -1;
  }
  int sourcePlanet = Integer.parseInt(tokens[0]);
  int destinationPlanet = Integer.parseInt(tokens[1]);
  int numShips = Integer.parseInt(tokens[2]);
  return IssueOrder(playerID, sourcePlanet, destinationPlanet, numShips);
    }

    // Kicks a player out of the game. This is used in cases where a player
    // tries to give an illegal order or runs over the time limit.
    public void DropPlayer(int playerID) {
	for (Planet p : planets) {
	    if (p.Owner() == playerID) {
		p.Owner(0);
	    }
	}
	for (Fleet f : fleets) {
	    if (f.Owner() == playerID) {
		f.Kill();
	    }
	}
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
	remainingPlayers.remove(0);
	if (numTurns > maxGameLength) {
	    int leadingPlayer = -1;
	    int mostShips = -1;
	    for (int playerID : remainingPlayers) {
		int numShips = NumShips(playerID);
		if (numShips == mostShips) {
		    leadingPlayer = 0;
		} else if (numShips > mostShips) {
		    leadingPlayer = playerID;
		    mostShips = numShips;
		}
	    }
	    return leadingPlayer;
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

    // Returns the game playback string. This is a complete record of the game,
    // and can be passed to a visualization program to playback the game.
    public String GamePlaybackString() {
  return gamePlayback;
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
  
  // Gets a color for a player (clamped)
  private Color GetColor(int player, ArrayList<Color> colors) {
    if (player > colors.size()) {
      return Color.PINK;
    } else {
      return colors.get(player);
    }
  }
  
  private Point getPlanetPos(Planet p, double top, double left,
                 double right, double bottom, int width,
                 int height) {
    int x = (int)((p.X() - left) / (right - left) * width);
    int y = height - (int)((p.Y() - top) / (bottom - top) * height);
    return new Point(x, y);
  }

  // A planet's inherent radius is its radius before being transformed for
  // rendering. The final rendered radii of all the planets are proportional
  // to their inherent radii. The radii are scaled for maximum aesthetic
  // appeal.
  private double inherentRadius(Planet p) {
    return Math.sqrt(p.GrowthRate());
    //return Math.log(p.GrowthRate() + 3.0);
    //return p.GrowthRate();
  }

  // Renders the current state of the game to a graphics object
  //
  // The offset is a number between 0 and 1 that specifies how far we are
  // past this game state, in units of time. As this parameter varies from
  // 0 to 1, the fleets all move in the forward direction. This is used to
  // fake smooth animation.
  //
  // On success, return an image. If something goes wrong, returns null.
  void Render(int width, // Desired image width
              int height, // Desired image height
              double offset, // Real number between 0 and 1
              BufferedImage bgImage, // Background image
              ArrayList<Color> colors, // Player colors
              Graphics2D g) { // Rendering context
    Font planetFont = new Font("Sans Serif", Font.BOLD, 12);
    Font fleetFont = new Font("Sans serif", Font.BOLD, 18);
    Color bgColor = new Color(188, 189, 172);
    Color textColor = Color.BLACK;
    if (bgImage != null) {
      g.drawImage(bgImage, 0, 0, null);
    }
    // Determine the dimensions of the viewport in game coordinates.
    double top = Double.MAX_VALUE;
    double left = Double.MAX_VALUE;
    double right = Double.MIN_VALUE;
    double bottom = Double.MIN_VALUE;
    for (Planet p : planets) {
      if (p.X() < left) left = p.X();
      if (p.X() > right) right = p.X();
      if (p.Y() > bottom) bottom = p.Y();
      if (p.Y() < top) top = p.Y();
    }
    double xRange = right - left;
    double yRange = bottom - top;
    double paddingFactor = 0.1;
    left -= xRange * paddingFactor;
    right += xRange * paddingFactor;
    top -= yRange * paddingFactor;
    bottom += yRange * paddingFactor;
    Point[] planetPos = new Point[planets.size()];
    g.setFont(planetFont);
    FontMetrics fm = g.getFontMetrics(planetFont);
    // Determine the best scaling factor for the sizes of the planets.
    double minSizeFactor = Double.MAX_VALUE;
    for (int i = 0; i < planets.size(); ++i) {
      for (int j = i + 1; j < planets.size(); ++j) {
        Planet a = planets.get(i);
        Planet b = planets.get(j);
        double dx = b.X() - a.X();
        double dy = b.Y() - a.Y();
        double dist = Math.sqrt(dx * dx + dy * dy);
        double aSize = inherentRadius(a);
        double bSize = inherentRadius(b);
        double sizeFactor = dist / (Math.sqrt(a.GrowthRate()));
        minSizeFactor = Math.min(sizeFactor, minSizeFactor);
      }
    }
    minSizeFactor *= 1.2;
    // Draw the planets.
    int i = 0;
    for (Planet p : planets) {
      Point pos = getPlanetPos(p, top, left, right, bottom, width,
                   height);
      planetPos[i++] = pos;
      int x = pos.x;
      int y = pos.y;
      double size = minSizeFactor * inherentRadius(p);
      int r = (int)Math.min(size / (right - left) * width,
                            size / (bottom - top) * height);
      g.setColor(GetColor(p.Owner(), colors));
      int cx = x - r / 2;
      int cy = y - r / 2;
      g.fillOval(cx, cy, r, r);
      Color c = g.getColor();
      for (int step = 1; step >= 0; step--) {
        g.setColor(g.getColor().brighter());
        g.drawOval(x - (r-step)/2, y - (r-step)/2, r-step, r-step);
      }
      g.setColor(c);
      for (int step = 0; step < 3; step++) {
        g.drawOval(x - (r+step)/2, y - (r+step)/2, r+step, r+step);
          g.setColor(g.getColor().darker());
      }
      
      java.awt.geom.Rectangle2D bounds =
        fm.getStringBounds(Integer.toString(p.NumShips()), g);
      x -= bounds.getWidth()/2;
      y += fm.getAscent()/2;
      
      g.setColor(textColor);
      g.drawString(Integer.toString(p.NumShips()), x, y);
    }
    // Draw fleets
    g.setFont(fleetFont);
    fm = g.getFontMetrics(fleetFont);
    for (Fleet f : fleets) {
      Point sPos = planetPos[f.SourcePlanet()];
      Point dPos = planetPos[f.DestinationPlanet()];
      double tripProgress =
        1.0 - (double)f.TurnsRemaining() / f.TotalTripLength();
      if (tripProgress > 0.99 || tripProgress < 0.01) {
	continue;
      }
      double dx = dPos.x - sPos.x;
      double dy = dPos.y - sPos.y;
      double x = sPos.x + dx * tripProgress;
      double y = sPos.y + dy * tripProgress;
      java.awt.geom.Rectangle2D textBounds =
        fm.getStringBounds(Integer.toString(f.NumShips()), g);
      g.setColor(GetColor(f.Owner(), colors).darker());
      g.drawString(Integer.toString(f.NumShips()),
                  (int)(x-textBounds.getWidth()/2),
                  (int)(y+textBounds.getHeight()/2));
    }
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
    if (gamePlayback.length() > 0) {
        gamePlayback += ":";
    }
    gamePlayback += "" + x + "," + y + "," + owner + "," +
        numShips + "," + growthRate;
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
  gamePlayback += "|";
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

    // The filename of the map that this game is being played on.
    private String mapFilename;

    // The string of map data to parse.
    private String mapData;

    // Stores a mode identifier which determines how to initialize this object.
    // See the constructor for details.
    private int initMode;

    // This is the game playback string. It's a complete description of the
    // game. It can be read by a visualization program to visualize the game.
    private String gamePlayback;

    // The maximum length of the game in turns. After this many turns, the game
    // will end, with whoever has the most ships as the winner. If there is no
    // player with the most ships, then the game is a draw.
    private int maxGameLength;
    private int numTurns;

    // This is the name of the file in which to write log messages.
    private String logFilename;
  
  private Game (Game _g) {
    planets = new ArrayList<Planet>();
    for (Planet p : _g.planets) {
      planets.add((Planet)(p.clone()));
    }
    fleets = new ArrayList<Fleet>();
    for (Fleet f : _g.fleets) {
      fleets.add((Fleet)(f.clone()));
    }
    if (_g.mapFilename != null)
      mapFilename = new String(_g.mapFilename);
    if (_g.mapData != null)
      mapData = new String(_g.mapData);
    initMode = _g.initMode;
    if (_g.gamePlayback != null)
      gamePlayback = new String(_g.gamePlayback);
    maxGameLength = _g.maxGameLength;
    numTurns = _g.numTurns;
    // Dont need to init the drawing stuff (it does it itself)
  }
  public Object clone() {
    return new Game(this);
  }
}
