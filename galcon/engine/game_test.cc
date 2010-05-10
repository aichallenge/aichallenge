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
// Unit tests for the Sandbox class.

#include "galcon/engine/game.h"
#include <sstream>
#include "gtest/gtest.h"

// This scenario has two fleets of equal strength, owned by different players,
// about to hit a single neutral planet with no ships occupying it. It's a
// decent edge case to use for testing the battle resolution code. The target
// planet should end up being neutral and occupied by no ships.
TEST(GameTest, BattleResolutionDraw) {
  std::stringstream map;
  map << "P 0 0 1 19 2" << std::endl;
  map << "P 2 4 2 19 2" << std::endl;
  map << "P 1 2 0 0 5" << std::endl;
  map << "F 1 15 0 2 3 1" << std::endl;
  map << "F 2 15 1 2 3 1" << std::endl;
  Game game(map.str(), 99, 1);
  game.Init();
  game.DoTimeStep();
  const Planet& p = game.GetPlanet(2);
  ASSERT_EQ(p.Owner(), 0);
  ASSERT_EQ(p.NumShips(), 0);
}

/*
// Tests the default constructor. It's supposed to make a standard 3x3 Tic Tac
// Toe board.
TEST(GridTest, DefaultConstructor) {
  Grid g;
  ASSERT_EQ(g.Width(), 3) << "Board width is suppsed to be 3.";
  ASSERT_EQ(g.Height(), 3) << "Board height is suppsed to be 3.";
  ASSERT_EQ(g.WinLength(), 3) << "Board win length is suppsed to be 3.";
  ASSERT_EQ(g.GameOver(), false) << "Game should not be over right after "
                                 << "initialization.";
}

// Tests the custom constructor, which creates boards with arbitrary
// dimensions.
TEST(GridTest, CustomConstructor) {
  Grid g(2, 4, 3);
  ASSERT_EQ(g.Width(), 2) << "Wrong board width.";
  ASSERT_EQ(g.Height(), 4) << "Wrong board height.";
  ASSERT_EQ(g.WinLength(), 3) << "Wrong win length.";
  ASSERT_EQ(g.GameOver(), false) << "Game should not be over right after "
                                 << "initialization.";  
}

// Runs a full game on a small, simple board, testing the state of the grid
// after each move.
TEST(GridTest, SmallGame) {
  Grid g(2, 2, 2);
  ASSERT_EQ(g.Width(), 2);
  ASSERT_EQ(g.Height(), 2);
  ASSERT_EQ(g.WinLength(), 2);
  ASSERT_EQ(g.GameOver(), false);
  ASSERT_EQ(g.WhoseTurn(), 1);
  std::stringstream grid_string;
  grid_string << "2 2 2" << std::endl
	      << "00" << std::endl
	      << "00" << std::endl;
  ASSERT_EQ(g.ToString(), grid_string.str());
  ASSERT_EQ(g.Winner(), 0);
  ASSERT_EQ(g.Loser(), 0);
  ASSERT_EQ(g.MakeMove(0, 0), 0);
  ASSERT_EQ(g.GameOver(), false);
  ASSERT_EQ(g.WhoseTurn(), 2);
  grid_string.str("");
  grid_string << "2 2 2" << std::endl
	      << "10" << std::endl
	      << "00" << std::endl;
  ASSERT_EQ(g.ToString(), grid_string.str());
  ASSERT_EQ(g.Winner(), 0);
  ASSERT_EQ(g.Loser(), 0);
  ASSERT_EQ(g.MakeMove(1, 0), 0);
  ASSERT_EQ(g.GameOver(), false);
  ASSERT_EQ(g.WhoseTurn(), 1);
  grid_string.str("");
  grid_string << "2 2 2" << std::endl
	      << "12" << std::endl
	      << "00" << std::endl;
  ASSERT_EQ(g.ToString(), grid_string.str());
  ASSERT_EQ(g.Winner(), 0);
  ASSERT_EQ(g.Loser(), 0);
  ASSERT_EQ(g.MakeMove(1, 1), 0);
  ASSERT_EQ(g.GameOver(), true);
  ASSERT_EQ(g.WhoseTurn(), 2);
  grid_string.str("");
  grid_string << "2 2 2" << std::endl
	      << "12" << std::endl
	      << "01" << std::endl;
  ASSERT_EQ(g.ToString(), grid_string.str());
  ASSERT_EQ(g.Winner(), 1);
  ASSERT_EQ(g.Loser(), 2);
}

// Tries to move onto a square which has already been filled.
TEST(GridTest, IllegalMove) {
  Grid g;
  ASSERT_EQ(g.WhoseTurn(), 1);
  ASSERT_EQ(g.MakeMove(1, 1), 0);
  ASSERT_EQ(g.WhoseTurn(), 2);
  ASSERT_EQ(g.MakeMove(1, 1), -1);
  ASSERT_EQ(g.WhoseTurn(), 2);
}

// Tries to move onto a square which is out of bounds.
TEST(GridTest, OutOfBounds) {
  Grid g;
  ASSERT_EQ(g.MakeMove(3, 2), -1);
}

// Makes sure that draws are detected okay. Plays the following game:
//   122
//   211
//   112
// where the moves are made in the following order:
//   984
//   615
//   372
// then checks to make sure that the draw is detected.
TEST(GridTest, DrawDetection) {
  Grid g;
  ASSERT_EQ(g.GameOver(), false);
  ASSERT_EQ(g.MakeMove(1, 1), 0);
  ASSERT_EQ(g.GameOver(), false);
  ASSERT_EQ(g.MakeMove(2, 2), 0);
  ASSERT_EQ(g.GameOver(), false);
  ASSERT_EQ(g.MakeMove(0, 2), 0);
  ASSERT_EQ(g.GameOver(), false);
  ASSERT_EQ(g.MakeMove(2, 0), 0);
  ASSERT_EQ(g.GameOver(), false);
  ASSERT_EQ(g.MakeMove(2, 1), 0);
  ASSERT_EQ(g.GameOver(), false);
  ASSERT_EQ(g.MakeMove(0, 1), 0);
  ASSERT_EQ(g.GameOver(), false);
  ASSERT_EQ(g.MakeMove(1, 2), 0);
  ASSERT_EQ(g.GameOver(), false);
  ASSERT_EQ(g.MakeMove(1, 0), 0);
  ASSERT_EQ(g.GameOver(), false);
  ASSERT_EQ(g.MakeMove(0, 0), 0);
  ASSERT_EQ(g.GameOver(), true);
  ASSERT_EQ(g.Winner(), 0);
  ASSERT_EQ(g.Loser(), 0);
}
*/
