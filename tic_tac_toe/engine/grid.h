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
// A generalizable Tic Tac Toe grid. Keeps track of who has moved where, whose
// turn it is, etc. Also checks to see if either player has won. The default
// dimensions are 3x3, and the win length is 3. These can be changed easily.

#ifndef TIC_TAC_TOE_ENGINE_GRID_H_
#define TIC_TAC_TOE_ENGINE_GRID_H_

#include <string>
#include <vector>

class Grid {
 public:
  // Creates an ordinary old 3x3 Tic Tac Toe grid.
  Grid();

  // Creates a Tic Tac Toe grid with the given width and height, and with the
  // given win length.
  Grid(int width, int height, int win_length);

  // Returns whose turn it is. The player numbers are 1 and 2.
  int WhoseTurn() const;

  // Accessors.
  int Width() const;
  int Height() const;
  int WinLength() const;

  // Returns a string representation of the board.
  std::string ToString() const;

  // Makes a move for the player whose turn it is. Returns 0 on success, or -1
  // if there is a problem. A problem could happen if the move is illegal, or
  // if the game is already over, etc.
  int MakeMove(int x, int y);

  // Returns true if the game is over. The game can end if either player has
  // won, or if the board is full and there are no more empty spaces. Otherwise
  // returns false.
  bool GameOver();

  // If the game is over, returns the winning player number. If the game is
  // not over, returns -1. If the game is drawn, returns 0.
  int Winner();

  // If the game is over, returns the losing player number. If the game is
  // not over, returns -1. If the game is drawn, returns 0.
  int Loser();

 private:
  // Returns true if the given coordinates are valid on this board, false
  // otherwise.
  bool OnBoard(int x, int y);

  // Checks to see if the game is over. If so, sets the game_over_ flag.
  void CheckGameOver();

  // A recursive method for checking the victory condition. Returns true iff
  // the first depth squares starting from (x,y) going in the direction (dx,dy)
  // have the same value as player. Basically, looks for depth in a row.
  bool CheckWin(int x, int y, int dx, int dy, int depth, int player);

  // True if the game has ended, false if the game has not ended. This flag is
  // updated after each move.
  bool game_over_;

  // The board itself is stored as a list of lists (actually, a vector of
  // vectors). Why isn't it just an int[3][3] instead? So that it can be
  // generalized to different board sizes, that's why.
  std::vector<std::vector<int> > squares_;

  // The dimensions of the grid.
  int width_;
  int height_;

  // This is the number of pieces that one played needs in a row in order to
  // win. Usually, this is 3. However, it's a variable here so that it can be
  // easily generalized.
  int win_length_;

  // Keeps track of whose turn it is.
  int whose_turn_;

  // If the game is over, stores the winning player's number and the losing
  // player's number. Player numbers are 1 or 2.
  int winner_;
  int loser_;
};

#endif
