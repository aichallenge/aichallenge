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

#ifndef PLANET_WARS_ENGINE_GAME_H_
#define PLANET_WARS_ENGINE_GAME_H_

#include <string>
#include <vector>
#include "planet_wars/engine/fleet.h"
#include "planet_wars/engine/planet.h"
#include "sandbox/sandbox.h"

class Game {
 public:
  // There are two modes:
  //   * If mode == 0, then s is interpreted as a filename, and the game is
  //     initialized by reading map data out of the named file.
  //   * If mode == 1, then s is interpreted as a string that contains map data
  //     directly. The string is parsed in the same way that the contents of
  //     a map file would be.
  // This constructor does not actually initialize the game object. You must
  // always call Init() before the game object will be in any kind of
  // coherent state.
  Game(const std::string& s, int max_game_length, int mode = 0);

  // Initializes a game of Planet Wars. Loads the map data from the file
  // specified in the constructor. Returns 1 on success, 0 on failure.
  int Init();

  // Returns the number of planets. Planets are numbered starting with 0.
  int NumPlanets() const;

  // Returns the planet with the given planet_id. There are NumPlanets()
  // planets. They are numbered starting at 0.
  const Planet& GetPlanet(int planet_id) const;

  // Returns the number of fleets.
  int NumFleets() const;

  // Returns the fleet with the given fleet_id. Fleets are numbered starting
  // with 0. There are NumFleets() fleets. fleet_id's are not consistent from
  // one turn to the next.
  const Fleet& GetFleet(int fleet_id) const;

  // Writes a string which represents the current game state. This string
  // conforms to the Point-in-Time format from the project Wiki.
  //
  // Optionally, you may specify the pov (Point of View) parameter. The pov
  // parameter is a player number. If specified, the player numbers 1 and pov
  // will be swapped in the game state output. This is used when sending the
  // game state to individual players, so that they can always assume that they
  // are player number 1.
  std::string ToString(int pov = -1) const;

  // Carries out the point-of-view switch operation, so that each player can
  // always assume that he is player number 1. There are three cases.
  // 1. If pov < 0 then no pov switching is being used. Return player_id.
  // 2. If player_id == pov then return 1 so that each player thinks he is
  //    player number 1.
  // 3. If player_id == 1 then return pov so that the real player 1 looks like
  //    he is player number "pov".
  // 4. Otherwise return player_id, since players other than 1 and pov are
  //    unaffected by the pov switch.
  static int PovSwitch(int pov, int player_id);

  // Returns the distance between two planets, rounded up to the next highest
  // integer. This is the number of discrete time steps it takes to get between
  // the two planets.
  int Distance(int source_planet, int destination_planet) const;

  // Executes one time step.
  //   * Planet bonuses are added to non-neutral planets.
  //   * Fleets are advanced towards their destinations.
  //   * Fleets that arrive at their destination are dealt with.
  void DoTimeStep();

  // Issue an order. This function takes num_ships off the source_planet, puts
  // them into a newly-created fleet, calculates the distance to the
  // destination_planet, and sets the fleet's total trip time to that distance.
  // Checks that the given player_id is allowed to give the given order. If
  // not, the offending player is kicked from the game. If the order was
  // carried out without any issue, and everything is peachy, then 0 is
  // returned. Otherwise, -1 is returned.
  int IssueOrder(int player_id,
		 int source_planet,
		 int destination_planet,
		 int num_ships);

  // Behaves just like the longer form of IssueOrder, but takes a string
  // of the form "source_planet destination_planet num_ships". That is, three
  // integers separated by space characters.
  int IssueOrder(int player_id, const std::string& order);

  // Kicks a player out of the game. This is used in cases where a player tries
  // to give an illegal order or runs over the time limit.
  void DropPlayer(int player_id);

  // Returns true if the named player owns at least one planet or fleet.
  // Otherwise, the player is deemed to be dead and false is returned.
  bool IsAlive(int player_id) const;

  // If the game is not yet over (ie: at least two players have planets or
  // fleets remaining), returns -1. If the game is over (ie: only one player
  // is left) then that player's number is returned. If there are no remaining
  // players, then the game is a draw and 0 is returned.
  int Winner() const;

  // Returns the game playback string. This is a complete record of the game,
  // and can be passed to a visualization program to playback the game.
  std::string GamePlaybackString() const;

  // Returns the number of ships that the current player has, either located
  // on planets or in flight.
  int NumShips(int player_id) const;

 private:
  // Parses a game state from a string. On success, returns 1. On failure,
  // returns 0.
  int ParseGameState(const std::string& s);

  // Loads a map from a test file. The text file contains a description of the
  // starting state of a game. See the project wiki for a description of the
  // file format. It should be called the Planet Wars Point-in-Time format. On
  // success, return 1. On failure, returns 0.
  int LoadMapFromFile(const std::string& map_filename);

  // Store all the planets and fleets. OMG we wouldn't wanna lose all the
  // planets and fleets, would we!?
  std::vector<Planet> planets_;
  std::vector<Fleet> fleets_;

  // The filename of the map that this game is being played on.
  std::string map_filename_;

  // The string of map data to parse.
  std::string map_data_;

  // Stores a mode identifier which determines how to initialize this object.
  // See the constructor for details.
  int init_mode_;

  // This is the game playback string. It's a complete description of the game.
  // It can be read by a visualization program to visualize the game.
  std::string game_playback_;

  // The maximum length of the game in turns. After this many turns, the game
  // will end, with whoever has the most ships as the winner. If there is no
  // player with the most ships, then the game is a draw.
  int max_game_length_;
  int num_turns_;
  bool game_over_;
};

#endif
