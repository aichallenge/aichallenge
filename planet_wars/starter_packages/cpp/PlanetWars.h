// This file contains helper code that does all the boring stuff for you.
// The code in this file takes care of storing lists of planets and fleets, as
// well as communicating with the game engine. You can get along just fine
// without ever looking at this file. However, you are welcome to modify it
// if you want to.
#ifndef PLANET_WARS_H_
#define PLANET_WARS_H_

#include <string>
#include <vector>

class StringUtil {
 public:
  // Tokenizes a string s into tokens. Tokens are delimited by any of the
  // characters in delimiters. Blank tokens are omitted.
  static void Tokenize(const std::string& s,
                       const std::string& delimiters,
                       std::vector<std::string>& tokens);

  // A more convenient way of calling the Tokenize() method.
  static std::vector<std::string> Tokenize(
                       const std::string& s,
                       const std::string& delimiters = std::string(" "));
};

class Fleet {
 public:
  // Initializes a fleet.
  Fleet(int owner,
        int num_ships,
        int source_planet = -1,
        int destination_planet = -1,
        int total_trip_length = -1,
        int turns_remaining = -1);

  // Accessors and simple modification functions. These should be mostly
  // self-explanatory.
  int Owner() const;
  int NumShips() const;
  int SourcePlanet() const;
  int DestinationPlanet() const;
  int TotalTripLength() const;
  int TurnsRemaining() const;
  void RemoveShips(int amount);

  // Subtracts one turn remaining. Call this function to make the fleet get
  // one turn closer to its destination.
  void TimeStep();

  // A comparison operator so that vectors of fleets can be sorted according to
  // their strength.
  bool operator<(const Fleet& f) const;

 private:
  int owner_;
  int num_ships_;
  int source_planet_;
  int destination_planet_;
  int total_trip_length_;
  int turns_remaining_;
};

class Planet {
 public:
  // Initializes a planet.
  Planet(int owner, int num_ships, int growth_rate, double x, double y);

  // Accessors and simple modification functions. These should be mostly
  // self-explanatory.
  int Owner() const;
  int NumShips() const;
  int GrowthRate() const;
  double X() const;
  double Y() const;
  void Owner(int new_owner);
  void NumShips(int new_num_ships);
  void AddShips(int amount);
  void RemoveShips(int amount);

 private:
  int owner_;
  int num_ships_;
  int growth_rate_;
  double x_, y_;
};

class PlanetWars {
 public:
  // Initializes the game state given a string containing game state data.
  PlanetWars(const std::string& game_state);

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

  // Returns the distance between two planets, rounded up to the next highest
  // integer. This is the number of discrete time steps it takes to get between
  // the two planets.
  int Distance(int source_planet, int destination_planet) const;

  // Sends an order to the game engine. The order is to send num_ships ships
  // from source_planet to destination_planet. The order must be valid, or
  // else your bot will get kicked and lose the game. For example, you must own
  // source_planet, and you can't send more ships than you actually have on
  // that planet.
  void IssueOrder(int source_planet, int destination_planet, int num_ships);

  // Returns true if the named player owns at least one planet or fleet.
  // Otherwise, the player is deemed to be dead and false is returned.
  bool IsAlive(int player_id) const;

  // Returns the number of ships that the given player has, either located
  // on planets or in flight.
  int NumShips(int player_id) const;

 private:
  // Parses a game state from a string. On success, returns 1. On failure,
  // returns 0.
  int ParseGameState(const std::string& s);

  // Store all the planets and fleets. OMG we wouldn't wanna lose all the
  // planets and fleets, would we!?
  std::vector<Planet> planets_;
  std::vector<Fleet> fleets_;
};

#endif
