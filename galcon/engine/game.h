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

#ifndef GALCON_ENGINE_GAME_H_
#define GALCON_ENGINE_GAME_H_

#include <string>
#include <vector>
#include "galcon/engine/fleet.h"
#include "galcon/engine/planet.h"

class Game {
 public:
  // Initializes a game using a description of a map from a text file. See the
  // LoadMapFromFile function for a description of the map file format.
  Game(const std::string& map_filename);

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
  std::string ToString() const;

 private:
  // Parses a game state from a string.
  void ParseGameState(const std::string& s);

  // Loads a map from a test file. The text file contains a description of the
  // starting state of a game. See the project wiki for a description of the
  // file format. It should be called the Galcon Point-in-Time format.
  void LoadMapFromFile(const std::string& map_filename);

  // Store all the planets and fleets. OMG we wouldn't wanna lose all the
  // planets and fleets, would we!?
  std::vector<Planet> planets_;
  std::vector<Fleet> fleets_;
};

#endif
