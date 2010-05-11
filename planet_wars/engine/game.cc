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

#include "planet_wars/engine/game.h"
#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include "cpp_util/string_util.h"
#include "planet_wars/engine/fleet.h"
#include "planet_wars/engine/planet.h"

Game::Game(const std::string& s, int max_game_length, int mode) {
  init_mode_ = mode;
  switch (init_mode_) {
  case 0:
    map_filename_ = s;
    break;
  case 1:
    map_data_ = s;
    break;
  default:
    break;
  }
  max_game_length_ = max_game_length;
  num_turns_ = 0;
}

int Game::Init() {
  switch (init_mode_) {
  case 0:
    return LoadMapFromFile(map_filename_);
  case 1:
    return ParseGameState(map_data_);
  default:
    return 0;
  }
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

std::string Game::ToString(int pov) const {
  std::stringstream s;
  for (unsigned int i = 0; i < planets_.size(); ++i) {
    const Planet& p = planets_[i];
    s << "P " << p.X() << " " << p.Y() << " " << PovSwitch(pov, p.Owner())
      << " " << p.NumShips() << " " << p.GrowthRate() << std::endl;
  }
  for (unsigned int i = 0; i < fleets_.size(); ++i) {
    const Fleet& f = fleets_[i];
    s << "F " << PovSwitch(pov, f.Owner()) << " " << f.NumShips() << " "
      << f.SourcePlanet() << " " << f.DestinationPlanet() << " "
      << f.TotalTripLength() << " " << f.TurnsRemaining() << std::endl;
  }
  return s.str();
}

int Game::PovSwitch(int pov, int player_id) {
  if (pov < 0) return player_id;
  if (player_id == pov) return 1;
  if (player_id == 1) return pov;
  return player_id;
}


int Game::Distance(int source_planet, int destination_planet) const {
  const Planet& source = planets_[source_planet];
  const Planet& destination = planets_[destination_planet];
  double dx = source.X() - destination.X();
  double dy = source.Y() - destination.Y();
  return (int)ceil(sqrt(dx * dx + dy * dy));
}

void Game::DoTimeStep() {
  for (unsigned int i = 0; i < planets_.size(); ++i) {
    Planet& p = planets_[i];
    if (p.Owner() > 0) {
      p.AddShips(p.GrowthRate());
    }
  }
  // Advance all fleets by one time step. Collect the ones that are arriving
  // at their destination planets this turn. Group them by destination and
  // attacking player using the attackers map. For example, attackers[3][4]
  // will store how many of player 4's ships are landing on planet 3 this turn.
  std::map<int, std::map<int, int> > attackers;
  for (unsigned int i = 0; i < fleets_.size(); ++i) {
    Fleet& f = fleets_[i];
    f.TimeStep();
    if (f.TurnsRemaining() == 0) {
      const int dest = f.DestinationPlanet();
      const int attacker = f.Owner();
      if (attackers.count(dest) == 0) {
	attackers.insert(
	  std::pair<int, std::map<int, int> >(dest, std::map<int, int>()));
      }
      if (attackers[dest].count(attacker) == 0) {
	attackers[dest].insert(std::pair<int, int>(attacker, 0));
      }
      attackers[dest][attacker] += f.NumShips();
      fleets_.erase(fleets_.begin() + i);
      --i;
    }
  }
  // Resolve the status of each planet which is being attacked. This is non-
  // trivial, since a planet can be attacked by many different players at once.
  for (unsigned int i = 0; i < planets_.size(); ++i) {
    if (attackers.count(i) == 0) {
      continue;
    }
    //std::cout << "Resolving the status of planet " << i << std::endl;
    Planet& p = planets_[i];
    //std::cout << "owner: " << p.Owner()
    //          << " num_ships: " << p.NumShips() << std::endl;
    const int defender = p.Owner();
    // Add the current owner's "attacking" ships to the defending forces.
    if (attackers[i].count(defender) > 0) {
      p.AddShips(attackers[i][defender]);
      attackers[i].erase(defender);
    }
    // Empty the attackers into a vector of fleets and sort them from
    // weakest to strongest.
    std::vector<Fleet> enemy_fleets;
    std::map<int, int>::iterator iter;
    int num_enemy_ships = 0;
    for (iter = attackers[i].begin(); iter != attackers[i].end(); ++iter) {
      enemy_fleets.push_back(Fleet(iter->first, iter->second));
      num_enemy_ships += iter->second;
    }
    sort(enemy_fleets.begin(), enemy_fleets.end());
    /*std::cout << "== Forces pre-battle ==" << std::endl;
    std::cout << "  Defending forces:" << std::endl;
    std::cout << "    owner: " << p.Owner()
	      << " strength: " << p.NumShips() << std::endl;
    std::cout << "  Attacking forces:" << std::endl;
    for (unsigned int j = 0; j < enemy_fleets.size(); ++j) {
      std::cout << "    owner: " << enemy_fleets[j].Owner()
		<< " strength: " << enemy_fleets[j].NumShips() << std::endl;
		}*/
    // Starting with the weakest attacker, the attackers take turns chipping
    // away the defending forces one by one until there are either no more
    // defenders or no more attackers.
    unsigned int whose_turn = 0;
    while (p.NumShips() > 0 && num_enemy_ships > 0) {
      if (enemy_fleets[whose_turn].NumShips() > 0) {
	p.RemoveShips(1);
	enemy_fleets[whose_turn].RemoveShips(1);
	--num_enemy_ships;
	if (enemy_fleets[whose_turn].NumShips() == 0) {
	  enemy_fleets.erase(enemy_fleets.begin() + whose_turn);
	  --whose_turn;
	}
      }
      ++whose_turn;
      if (whose_turn >= enemy_fleets.size()) {
	whose_turn = 0;
      }
    }
    /*std::cout << "== Forces after stage 1 ==" << std::endl;
    std::cout << "  Defending forces:" << std::endl;
    std::cout << "    owner: " << p.Owner()
	      << " strength: " << p.NumShips() << std::endl;
    std::cout << "  Attacking forces:" << std::endl;
    for (unsigned int j = 0; j < enemy_fleets.size(); ++j) {
      std::cout << "    owner: " << enemy_fleets[j].Owner()
		<< " strength: " << enemy_fleets[j].NumShips() << std::endl;
		}*/
    // If there are no enemy fleets left, then the defender keeps control of
    // the planet. If there are any enemy fleets left, then they battle it out
    // to determine who gets control of the planet. This is done by cycling
    // through the attackers and subtracting one ship at a time from each,
    // until there is only one attacker left. If the last attackers are all
    // eliminated at the same time, then the planet becomes neutral with zero
    // ships occupying it.
    if (num_enemy_ships > 0) {
      p.Owner(0);
      while (true) {
	for (unsigned int j = 0; j < enemy_fleets.size(); ++j) {
	  enemy_fleets[j].RemoveShips(1);
	  if (enemy_fleets[j].NumShips() <= 0) {
	    enemy_fleets.erase(enemy_fleets.begin() + j);
	    --j;
	  }
	}
	if (enemy_fleets.size() == 0) {
	  break;
	}
	if (enemy_fleets.size() == 1) {
	  p.Owner(enemy_fleets[0].Owner());
	  p.NumShips(enemy_fleets[0].NumShips());
	  break;
	}
      }
    }
    /*std::cout << "== Forces after resolution ==" << std::endl;
    std::cout << "  Defending forces:" << std::endl;
    std::cout << "    owner: " << p.Owner()
	      << " strength: " << p.NumShips() << std::endl;
    std::cout << "  Attacking forces:" << std::endl;
    for (unsigned int j = 0; j < enemy_fleets.size(); ++j) {
      std::cout << "    owner: " << enemy_fleets[j].Owner()
		<< " strength: " << enemy_fleets[j].NumShips() << std::endl;
		}*/
  }
  game_playback_ += ":";
  // Check to see if the maximum number of turns has been reached.
  ++num_turns_;
}

int Game::IssueOrder(int player_id,
		     int source_planet,
		     int destination_planet,
		     int num_ships) {
  Planet& source = planets_[source_planet];
  if (source.Owner() != player_id || num_ships > source.NumShips()) {
    DropPlayer(player_id);
    return -1;
  }
  source.RemoveShips(num_ships);
  int distance = Distance(source_planet, destination_planet);
  Fleet f(source.Owner(),
	  num_ships,
	  source_planet,
	  destination_planet,
	  distance,
	  distance);
  fleets_.push_back(f);
  const char last_char = game_playback_[game_playback_.length() - 1];
  if (last_char != ':' && last_char != '|') {
    game_playback_ += ",";
  }
  std::stringstream playback;
  playback << source_planet << "." << destination_planet << "." << num_ships;
  game_playback_ += playback.str();
  return 0;
}

int Game::IssueOrder(int player_id, const std::string& order) {
  std::vector<std::string> tokens = StringUtil::Tokenize(order);
  if (tokens.size() != 3) {
    return -1;
  }
  return IssueOrder(player_id,
		    atoi(tokens[0].c_str()),
		    atoi(tokens[1].c_str()),
		    atoi(tokens[2].c_str()));
}

void Game::DropPlayer(int player_id) {
  for (unsigned int i = 0; i < planets_.size(); ++i) {
    if (planets_[i].Owner() == player_id) {
      planets_[i].Owner(0);
    }
  }
}

bool Game::IsAlive(int player_id) const {
  for (unsigned int i = 0; i < planets_.size(); ++i) {
    if (planets_[i].Owner() == player_id) {
      return true;
    }
  }
  for (unsigned int i = 0; i < fleets_.size(); ++i) {
    if (fleets_[i].Owner() == player_id) {
      return true;
    }
  }
  return false;
}

int Game::Winner() const {
  std::set<int> remaining_players;
  for (unsigned int i = 0; i < planets_.size(); ++i) {
    remaining_players.insert(planets_[i].Owner());
  }
  for (unsigned int i = 0; i < fleets_.size(); ++i) {
    remaining_players.insert(fleets_[i].Owner());
  }
  if (num_turns_ > max_game_length_) {
    int leading_player = -1;
    int most_ships = -1;
    std::set<int>::iterator iter;
    for (iter = remaining_players.begin();
	 iter != remaining_players.end();
	 ++iter) {
      const int num_ships = NumShips(*iter);
      if (num_ships == most_ships) {
	leading_player = 0;
      } else if (num_ships > most_ships) {
	leading_player = *iter;
	most_ships = num_ships;
      }
    }
    return leading_player;
  }
  switch (remaining_players.size()) {
  case 0:
    return 0;
  case 1:
    return *remaining_players.begin();
  default:
    return -1;
  }
}

std::string Game::GamePlaybackString() const {
  return game_playback_.substr(0, game_playback_.length() - 1);
}

int Game::NumShips(int player_id) const {
  int num_ships = 0;
  for (unsigned int i = 0; i < planets_.size(); ++i) {
    if (planets_[i].Owner() == player_id) {
      num_ships += planets_[i].NumShips();
    }
  }
  for (unsigned int i = 0; i < fleets_.size(); ++i) {
    if (fleets_[i].Owner() == player_id) {
      num_ships += fleets_[i].NumShips();
    }
  }
  return num_ships;
}

int Game::ParseGameState(const std::string& s) {
  planets_.clear();
  fleets_.clear();
  std::vector<std::string> lines = StringUtil::Tokenize(s, "\n");
  for (unsigned int i = 0; i < lines.size(); ++i) {
    std::string& line = lines[i];
    size_t comment_begin = line.find_first_of('#');
    if (comment_begin != std::string::npos) {
      line = line.substr(0, comment_begin);
    }
    std::vector<std::string> tokens = StringUtil::Tokenize(line);
    if (tokens.size() == 0) {
      continue;
    }
    if (tokens[0] == "P") {
      if (tokens.size() != 6) {
	return 0;
      }
      Planet p(atoi(tokens[3].c_str()),  // Owner
	       atoi(tokens[4].c_str()),  // Num ships
	       atoi(tokens[5].c_str()),  // Growth rate
               atof(tokens[1].c_str()),  // X
	       atof(tokens[2].c_str())); // Y
      planets_.push_back(p);
      if (game_playback_.length() > 0) {
	game_playback_ += ":";
      }
      game_playback_ += tokens[1] + "," + tokens[2] + "," + tokens[3] + "," +
	tokens[4] + "," + tokens[5];
    } else if (tokens[0] == "F") {
      if (tokens.size() != 7) {
	return 0;
      }
      Fleet f(atoi(tokens[1].c_str()),  // Owner
	      atoi(tokens[2].c_str()),  // Num ships
	      atoi(tokens[3].c_str()),  // Source
	      atoi(tokens[4].c_str()),  // Destination
	      atoi(tokens[5].c_str()),  // Total trip length
	      atoi(tokens[6].c_str())); // Turns remaining
      fleets_.push_back(f);
    } else {
      return 0;
    }
  }
  game_playback_ += "|";
  return 1;
}

int Game::LoadMapFromFile(const std::string& map_filename) {
  std::ifstream f(map_filename.c_str(), std::ifstream::in);
  if (!f.is_open()) {
    return 0;
  }
  std::string s;
  int c;
  while ((c = f.get()) != EOF) {
    s += (char)c;
  }
  f.close();
  return ParseGameState(s);
}
