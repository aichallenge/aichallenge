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
// Represents a planet. A planet has
//   * an owner
//   * a number of ships
//   * a growth rate
//   * a position (x, y)

#ifndef PLANET_WARS_ENGINE_PLANET_H_
#define PLANET_WARS_ENGINE_PLANET_H_

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

#endif
