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

public class Planet implements Cloneable{
    // Initializes a planet.
    public Planet(int owner,
		  int numShips,
		  int growthRate,
		  double x,
		  double y) {
	this.owner = owner;
	this.numShips = numShips;
	this.growthRate = growthRate;
	this.x = x;
	this.y = y;
    }

    // Accessors and simple modification functions. These should be mostly
    // self-explanatory.
    public int Owner() {
	return owner;
    }

    public int NumShips() {
	return numShips;
    }

    public int GrowthRate() {
	return growthRate;
    }

    public double X() {
	return x;
    }

    public double Y() {
	return y;
    }

    public void Owner(int newOwner) {
	this.owner = newOwner;
    }

    public void NumShips(int newNumShips) {
	this.numShips = newNumShips;
    }

    public void AddShips(int amount) {
	numShips += amount;
    }

    public void RemoveShips(int amount) {
	numShips -= amount;
    }

    private int owner;
    private int numShips;
    private int growthRate;
    private double x, y;
	
	private Planet (Planet _p) {
		owner = _p.owner;
		numShips = _p.numShips;
		growthRate = _p.growthRate;
		x = _p.x;
		y = _p.y;
	}
	public Object clone() {
		return new Planet(this);
	}
}
