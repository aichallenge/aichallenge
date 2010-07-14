#include <iostream>
#include "PlanetWars.h"

bool NumFleetsForPlayer(int player_id, const PlanetWars& pw) {
  int num_fleets = 0;
  for (int i = 0; i < pw.NumFleets(); ++i) {
    num_fleets += pw.GetFleet(i).Owner() == player_id ? 1 : 0;
  }
  return num_fleets;
}

void DoTurn(const PlanetWars& pw) {
  if (NumFleetsForPlayer(1, pw) > 0) {
    std::cout << "go" << std::endl;
    std::cout.flush();
    return;
  }
  int my_strongest_planet = -1;
  int my_score = -1;
  int other_weakest_planet = -1;
  int other_score = 999999999;
  for (int i = 0; i < pw.NumPlanets(); ++i) {
    const Planet& p = pw.GetPlanet(i);
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
        PlanetWars pw(map_data);
        map_data = "";
        DoTurn(pw);
      } else {
        map_data += current_line;
      }
      current_line = "";
    }
  }
  return 0;
}
