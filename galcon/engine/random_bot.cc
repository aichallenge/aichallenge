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
// A basic Galcon bot which uses a very simple strategy. It sends one fleet at
// a time from its strongest planet to the weakest planet that it doesn't own.
// If it already has a fleet traveling between planets, it does nothing. The
// amount of ships sent is half the number of ships available on the source
// planet. The idea behind this strategy is to try to build more production
// capacity as cheaply as possible, and eventually to erode opp's production
// capacity as cheaply as possible.

#include <climits>
#include <iostream>
#include "galcon/engine/game.h"

bool NumFleetsForPlayer(int player_id, const Game& game) {
  int num_fleets = 0;
  for (int i = 0; i < game.NumFleets(); ++i) {
    num_fleets += game.GetFleet(i).Owner() == player_id ? 1 : 0;
  }
  return num_fleets;
}

void DoTurn(const Game& game) {
  if (NumFleetsForPlayer(1, game) > 0) {
    std::cout << "go" << std::endl;
    std::cout.flush();
    return;
  }
  int my_strongest_planet = -1;
  int my_score = -1;
  int other_weakest_planet = -1;
  int other_score = INT_MAX;
  for (int i = 0; i < game.NumPlanets(); ++i) {
    const Planet& p = game.GetPlanet(i);
    if (p.Owner() == 1) {
      if (p.NumShips() > my_score) {
	my_score = p.NumShips();
	my_strongest_planet = i;
      }
    } else {
      if (p.NumShips() < other_score) {
	other_score = p.NumShips();
	other_weakest_planet = i;
      }
    }
  }
  if (my_strongest_planet >= 0 && other_weakest_planet >= 0) {
    std::cout << my_strongest_planet << " " << other_weakest_planet << " "
	      << (my_score / 2) << std::endl;
  }
  std::cout << "go" << std::endl;
  std::cout.flush();
}

int main(int argc, char *argv[]) {
  std::string current_line;
  std::string map_data;
  while (true) {
    int c = std::cin.get();
    current_line += (char)c;
    if (c == '\n') {
      if (current_line.length() >= 2 && current_line.substr(0, 2) == "go") {
	Game game(map_data, 0, 1);
	game.Init();
	map_data = "";
	DoTurn(game);
      } else {
	map_data += current_line;
      }
      current_line = "";
    }
  }
  return 0;
}
