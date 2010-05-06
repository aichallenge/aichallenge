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
// Represents a fleet. A fleet has
//   * an owner
//   * a number of ships
//   * a source planet
//   * a destination
//   * a total trip length
//   * a number of turns remaining before arrival

#ifndef GALCON_ENGINE_FLEET_H_
#define GALCON_ENGINE_FLEET_H_

class Fleet {
 public:
  // Initializes a planet.
  Fleet(int owner,
	int num_ships,
	int source_planet,
	int destination_planet,
	int total_trip_length,
	int turns_remaining);

  // Accessors and simple modification functions. These should be mostly
  // self-explanatory.
  int Owner() const;
  int NumShips() const;
  int SourcePlanet() const;
  int DestinationPlanet() const;
  int TotalTripLength() const;
  int TurnsRemaining() const;

  // Subtracts one turn remaining. Call this function to make the fleet get
  // one turn closer to its destination.
  void TimeStep();

 private:
  int owner_;
  int num_ships_;
  int source_planet_;
  int destination_planet_;
  int total_trip_length_;
  int turns_remaining_;
};

#endif
