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
// Stores the state of a game of Go. Represents the grid, the positions of
// stones, the number of captured stones, and other elements of the state of
// a game of Go.

#ifndef GO_ENGINE_GAME_H_
#define GO_ENGINE_GAME_H_

#include <string>
#include <vector>

class Game {
 public:
  // Initializes a game of Go. By default, a 19x19 game is created, but any
  // board size can be used.
  Game(int width = 19, int height = 19);

  // Returns whose turn it is. The player numbers are 1 and 2.
  int WhoseTurn() const;

  // Accessors.
  int Width() const;
  int Height() const;

  // Returns a string representation of the board as a grid of ASCII
  // characters. Each row of the board is represented by a sequence of
  // characters, with rows being separated by newlines. Empty spaces are
  // represented as the '.' character, player 1's stones are character '1', and
  // player 2's stones are character '2'. Optionally, pass perspective=2 to
  // reverse the player numbers.
  std::string AsciiRepresentation() const;

  // Makes a move for the player whose turn it is. Returns 0 on success, or -1
  // if there is a problem. A problem could happen if the move is illegal, or
  // if the game is already over, etc.
  int MakeMove(int x, int y);

  // Returns true if the game is over. The game is over if both players pass
  // their turns in a row.
  bool GameOver() const;

  // If the game is over, returns the winning player number. If the game is
  // not over, returns -1. If the game is drawn, returns 0.
  int Winner() const;

  // If the game is over, returns the losing player number. If the game is
  // not over, returns -1. If the game is drawn, returns 0.
  int Loser() const;

  // Returns the final score of the game. This is
  // (player_one_score - player_two_score). If it's positive, player one wins.
  // If it's negative, player two wins. If it's zero, player two wins. This
  // only works if the game is over.
  int NetScore() const;

  // Returns the first player's score. This only works if the game is over.
  int PlayerOneScore() const;

  // Return the second player's score. This only works if the game is over.
  int PlayerTwoScore() const;

 private:
  // Scores the game. The score can be retrieved using the NetScore() function.
  // This function may work by writing the board out to a file and calling an
  // external program to score the position. This is because scoring a final Go
  // position is extremely complicated. Why reinvent the wheel?
  void CalculateScore();

  // Returns true if the given coordinates are valid on this board, false
  // otherwise.
  bool OnBoard(int x, int y);

  // True if the game has ended, false if the game has not ended. This flag is
  // updated after each move.
  bool game_over_;

  // The board itself is stored as a list of lists (actually, a vector of
  // vectors).
  std::vector<std::vector<int> > grid_;

  // The dimensions of the grid.
  int width_;
  int height_;

  // Keeps track of whose turn it is. The player numbers are 1 and 2.
  int whose_turn_;

  // If the game is over, stores the winning player's number and the losing
  // player's number. Player numbers are 1 or 2.
  int winner_;
  int loser_;
};

#endif
