#include "PlanetWars.h"
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

void StringUtil::Tokenize(const std::string& s,
			  const std::string& delimiters,
			  std::vector<std::string>& tokens) {
  std::string::size_type lastPos = s.find_first_not_of(delimiters, 0);
  std::string::size_type pos = s.find_first_of(delimiters, lastPos);
  while (std::string::npos != pos || std::string::npos != lastPos) {
    tokens.push_back(s.substr(lastPos, pos - lastPos));
    lastPos = s.find_first_not_of(delimiters, pos);
    pos = s.find_first_of(delimiters, lastPos);
  }
}

std::vector<std::string> StringUtil::Tokenize(const std::string& s,
					      const std::string& delimiters) {
  std::vector<std::string> tokens;
  Tokenize(s, delimiters, tokens);
  return tokens;
}

Fleet::Fleet(int owner,
	     int num_ships,
	     int source_planet,
	     int destination_planet,
	     int total_trip_length,
	     int turns_remaining) {
  owner_ = owner;
  num_ships_ = num_ships;
  source_planet_ = source_planet;
  destination_planet_ = destination_planet;
  total_trip_length_ = total_trip_length;
  turns_remaining_ = turns_remaining;
}

int Fleet::Owner() const {
  return owner_;
}

int Fleet::NumShips() const {
  return num_ships_;
}

int Fleet::SourcePlanet() const {
  return source_planet_;
}

int Fleet::DestinationPlanet() const {
  return destination_planet_;
}

int Fleet::TotalTripLength() const {
  return total_trip_length_;
}

int Fleet::TurnsRemaining() const {
  return turns_remaining_;
}

void Fleet::RemoveShips(int amount) {
  num_ships_ -= amount;
  if (num_ships_ < 0) {
    num_ships_ = 0;
  }
}

void Fleet::TimeStep() {
  if (turns_remaining_ > 0) {
    --turns_remaining_;
  } else {
    turns_remaining_ = 0;
  }
}

bool Fleet::operator<(const Fleet& f) const {
  return num_ships_ < f.num_ships_;
}

Planet::Planet(int owner, int num_ships, int growth_rate, double x, double y) {
  owner_ = owner;
  num_ships_ = num_ships;
  growth_rate_ = growth_rate;
  x_ = x;
  y_ = y;
}

int Planet::Owner() const {
  return owner_;
}

int Planet::NumShips() const {
  return num_ships_;
}

int Planet::GrowthRate() const {
  return growth_rate_;
}

double Planet::X() const {
  return x_;
}

double Planet::Y() const {
  return y_;
}

void Planet::Owner(int new_owner) {
  owner_ = new_owner;
}

void Planet::NumShips(int new_num_ships) {
  num_ships_ = new_num_ships;
}

void Planet::AddShips(int amount) {
  num_ships_ += amount;
}

void Planet::RemoveShips(int amount) {
  num_ships_ -= amount;
}

PlanetWars::PlanetWars(const std::string& gameState) {
  ParseGameState(gameState);
}

int PlanetWars::NumPlanets() const {
  return planets_.size();
}

const Planet& PlanetWars::GetPlanet(int planet_id) const {
  return planets_[planet_id];
}

int PlanetWars::NumFleets() const {
  return fleets_.size();
}

const Fleet& PlanetWars::GetFleet(int fleet_id) const {
  return fleets_[fleet_id];
}

std::string PlanetWars::ToString() const {
  std::stringstream s;
  for (unsigned int i = 0; i < planets_.size(); ++i) {
    const Planet& p = planets_[i];
    s << "P " << p.X() << " " << p.Y() << " " << p.Owner()
      << " " << p.NumShips() << " " << p.GrowthRate() << std::endl;
  }
  for (unsigned int i = 0; i < fleets_.size(); ++i) {
    const Fleet& f = fleets_[i];
    s << "F " << f.Owner() << " " << f.NumShips() << " "
      << f.SourcePlanet() << " " << f.DestinationPlanet() << " "
      << f.TotalTripLength() << " " << f.TurnsRemaining() << std::endl;
  }
  return s.str();
}

int PlanetWars::Distance(int source_planet, int destination_planet) const {
  const Planet& source = planets_[source_planet];
  const Planet& destination = planets_[destination_planet];
  double dx = source.X() - destination.X();
  double dy = source.Y() - destination.Y();
  return (int)ceil(sqrt(dx * dx + dy * dy));
}

void PlanetWars::IssueOrder(int source_planet,
			    int destination_planet,
			    int num_ships) {
  std::cout << source_planet << " "
	    << destination_planet << " "
	    << num_ships << std::endl;
  std::cout.flush();
}

bool PlanetWars::IsAlive(int player_id) const {
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

int PlanetWars::ParseGameState(const std::string& s) {
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
  return 1;
}
