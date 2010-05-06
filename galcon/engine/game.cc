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

#include "galcon/engine/game.h"
#include <assert.h>
#include <sstream>
#include "cpp_util/string_util.h"
#include "galcon/engine/fleet.h"
#include "galcon/engine/planet.h"

Game::Game(const std::string& map_filename) {
  LoadMapFromFile(map_filename);
}

int Game::NumPlanets() const {
  return planets_.size();
}

const Planet& Game::GetPlanet(int planet_id) const {
  return planets_[planet_id];
}

int Game::NumFleets() const {
  return fleets_.size();
}

const Fleet& Game::GetFleet(int fleet_id) const {
  return fleets_[fleet_id];
}

std::string Game::ToString() const {
  std::stringstream s;
  for (unsigned int i = 0; i < planets_.size(); ++i) {
    const Planet& p = planets_[i];
    s << "P " << p.X() << " " << p.Y() << " " << p.Owner() << " "
      << p.NumShips() << " " << p.GrowthRate() << std::endl;
  }
  for (unsigned int i = 0; i < fleets_.size(); ++i) {
    const Fleet& f = fleets_[i];
    s << "F " << f.Owner() << " " << f.NumShips() << " "
      << f.SourcePlanet() << " " << f.DestinationPlanet() << " "
      << f.TotalTripLength() << " " << f.TurnsRemaining() << std::endl;
  }
  return s.str();
}

void Game::ParseGameState(const std::string& s) {
  planets_.clear();
  fleets_.clear();
  std::vector<std::string> lines = StringUtil::Tokenize(s, "\n");
  for (unsigned int i = 0; i < lines.size(); ++i) {
    std::string line = lines[i];
    size_t comment_begin = line.find_first_of('#');
    if (comment_begin != std::string::npos) {
      line = line.substr(0, comment_begin);
    }
    std::vector<std::string> tokens = StringUtil::Tokenize(line);
    if (tokens.size() == 0) {
      continue;
    }
    assert(tokens[0] == "P" || tokens[0] == "F");
    if (tokens[0] == "P") {
      assert(tokens.size(), 6);
      Planet p(atoi(tokens[3]),  // Owner
	       atoi(tokens[4]),  // Num ships
	       atoi(tokens[5]),  // Growth rate
	       atod(tokens[1]),  // X
	       atod(tokens[2])); // Y
      planets_.push_back(p);
    } else if (tokens[0] == "F") {
      assert(tokens.size(), 7);
      Fleet f(atoi(tokens[1]),  // Owner
	       atoi(tokens[2]),  // Num ships
	       atoi(tokens[3]),  // Source
	       atoi(tokens[4]),  // Destination
	       atoi(tokens[5]),  // Total trip length
	       atoi(tokens[6])); // Turns remaining
      fleets_.push_back(f);
    }
  }
}

void Game::LoadMapFromFile(const std::string& map_filename) {
  ifstream f(map_filename, ifstream::in);
  std::string s;
  int c;
  while ((c = f.get()) != EOF) {
    s += (char)c;
  }
  f.close();
  ParseGameState(s);
}
