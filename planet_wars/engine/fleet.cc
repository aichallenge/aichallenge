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

#include "planet_wars/engine/fleet.h"

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
