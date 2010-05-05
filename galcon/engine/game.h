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

class Game {
 public:
  // Initializes a game using a description of a map from a text file. See the
  // LoadMapFromFile function for a description of the map file format.
  Game(const std::string& map_filename);

  // Returns the number of planets in the map.
  int NumPlanets() const;

  // Returns the owner of the planet with the given number.
  int Owner(int planet_id) const;

  // Returns the number of ships in the given planet.
  int NumShips(int planet_id) const;

  // Returns the x- and y-coordinate of a given planet.
  int PlanetX(int planet_id) const;
  int PlanetY(int planet_id) const;

 private:
  // Loads a map from a test file. The text file contains a description of the
  // starting state of a game. One planet is specified per line. Each line
  // contains the x- and y-coordinate of one planet, the player number that
  // starts off owning the planet, and the number of ships originally in the
  // planet. Any characters on a line beyond a '#' character should be
  // ignored. The coordinates are real numbers, and need not be integers.
  // The player number and number of ships must be integers. Fields are
  // separated by space characters. Lines that do not appear to match the
  // specified line format should be ignored. For example:
  //
  // # This is a sample map file, for the purpose of illustration.
  // 0 0 1 34 # Player one home planet
  // 7 7 2 34 # Player two home planet
  // 3.14 2.71 0 15 # Neutral planet with real-numbere coordinates.
  void LoadMapFromFile(const std::string& map_filename);
};

#endif
