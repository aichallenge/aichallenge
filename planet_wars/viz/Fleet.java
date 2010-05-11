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

public class Fleet {
    // Initializes a fleet.
    public Fleet(int owner,
		 int numShips,
		 int sourcePlanet,
		 int destinationPlanet,
		 int totalTripLength,
		 int turnsRemaining) {
	this.owner = owner;
	this.numShips = numShips;
	this.sourcePlanet = sourcePlanet;
	this.destinationPlanet = destinationPlanet;
	this.totalTripLength = totalTripLength;
	this.turnsRemaining = turnsRemaining;
    }

    // Accessors and simple modification functions. These should be mostly
    // self-explanatory.
    public int Owner() {
	return owner;
    }

    public int NumShips() {
	return numShips;
    }

    public int SourcePlanet() {
	return sourcePlanet;
    }

    public int DestinationPlanet() {
	return destinationPlanet;
    }

    public int TotalTripLength() {
	return totalTripLength;
    }

    public int TurnsRemaining() {
	return turnsRemaining;
    }

    public void RemoveShips(int amount) {
	numShips -= amount;
    }

    // Subtracts one turn remaining. Call this function to make the fleet get
    // one turn closer to its destination.
    public void TimeStep() {
	if (turnsRemaining > 0) {
	    --turnsRemaining;
	} else {
	    turnsRemaining = 0;
	}
    }

    private int owner;
    private int numShips;
    private int sourcePlanet;
    private int destinationPlanet;
    private int totalTripLength;
    private int turnsRemaining;
}
