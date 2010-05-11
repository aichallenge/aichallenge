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
// Author: Jeff Cameron (jeff@jpcameron.com)
//
// Stores the game state.

import java.util.*;

public class Game {
    // There are two modes:
    //   * If mode == 0, then s is interpreted as a filename, and the game is
    //     initialized by reading map data out of the named file.
    //   * If mode == 1, then s is interpreted as a string that contains map data
    //     directly. The string is parsed in the same way that the contents of
    //     a map file would be.
    // This constructor does not actually initialize the game object. You must
    // always call Init() before the game object will be in any kind of
    // coherent state.
    public Game(String s, int maxGameLength, int mode) {
	
    }

    // Initializes a game of Planet Wars. Loads the map data from the file
    // specified in the constructor. Returns 1 on success, 0 on failure.
    public int Init() {
	return 0;
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
    // game state to individual players, so that they can always assume that they
    // are player number 1.
    public String PovRepresentation(int pov) {
	return null;
    }

    // Carries out the point-of-view switch operation, so that each player can
    // always assume that he is player number 1. There are three cases.
    // 1. If pov < 0 then no pov switching is being used. Return player_id.
    // 2. If player_id == pov then return 1 so that each player thinks he is
    //    player number 1.
    // 3. If player_id == 1 then return pov so that the real player 1 looks like
    //    he is player number "pov".
    // 4. Otherwise return player_id, since players other than 1 and pov are
    //    unaffected by the pov switch.
    public static int PovSwitch(int pov, int playerID) {
	return 0;
    }

    // Returns the distance between two planets, rounded up to the next highest
    // integer. This is the number of discrete time steps it takes to get between
    // the two planets.
    public int Distance(int sourcePlanet, int destinationPlanet) {
	return 0;
    }

    // Executes one time step.
    //   * Planet bonuses are added to non-neutral planets.
    //   * Fleets are advanced towards their destinations.
    //   * Fleets that arrive at their destination are dealt with.
    public void DoTimeStep() {
	
    }

    // Issue an order. This function takes num_ships off the source_planet, puts
    // them into a newly-created fleet, calculates the distance to the
    // destination_planet, and sets the fleet's total trip time to that distance.
    // Checks that the given player_id is allowed to give the given order. If
    // not, the offending player is kicked from the game. If the order was
    // carried out without any issue, and everything is peachy, then 0 is
    // returned. Otherwise, -1 is returned.
    public int IssueOrder(int player_id,
			  int source_planet,
			  int destination_planet,
			  int num_ships) {
	return 0;
    }

    // Behaves just like the longer form of IssueOrder, but takes a string
    // of the form "source_planet destination_planet num_ships". That is, three
    // integers separated by space characters.
    public int IssueOrder(int playerID, String order) {
	return 0;
    }

    // Kicks a player out of the game. This is used in cases where a player tries
    // to give an illegal order or runs over the time limit.
    public void DropPlayer(int playerID) {
	
    }

    // Returns true if the named player owns at least one planet or fleet.
    // Otherwise, the player is deemed to be dead and false is returned.
    public boolean IsAlive(int player_id) {
	return false;
    }

    // If the game is not yet over (ie: at least two players have planets or
    // fleets remaining), returns -1. If the game is over (ie: only one player
    // is left) then that player's number is returned. If there are no remaining
    // players, then the game is a draw and 0 is returned.
    public int Winner() {
	return -1;
    }

    // Returns the game playback string. This is a complete record of the game,
    // and can be passed to a visualization program to playback the game.
    public String GamePlaybackString() {
	return null;
    }

    // Returns the number of ships that the current player has, either located
    // on planets or in flight.
    public int NumShips(int player_id) {
	return -1;
    }

    // Renders the current state of the game as an image.
    java.awt.Image Render() {
	return null;
    }

    // Parses a game state from a string. On success, returns 1. On failure,
    // returns 0.
    private int ParseGameState(String s) {
	return 0;
    }

    // Loads a map from a test file. The text file contains a description of the
    // starting state of a game. See the project wiki for a description of the
    // file format. It should be called the Planet Wars Point-in-Time format.
    // On success, return 1. On failure, returns 0.
    private int LoadMapFromFile(String map_filename) {
	return 0;
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

    // This is the game playback string. It's a complete description of the game.
    // It can be read by a visualization program to visualize the game.
    private String gamePlayback;

    // The maximum length of the game in turns. After this many turns, the game
    // will end, with whoever has the most ships as the winner. If there is no
    // player with the most ships, then the game is a draw.
    private int maxGameLength;
    private int numTurns;
    private boolean gameOver;
}
