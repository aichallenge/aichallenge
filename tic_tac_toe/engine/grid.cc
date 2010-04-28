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

#include "tic_tac_toe/engine/grid.h"
#include <iostream>
#include <sstream>

Grid::Grid() {
  width_ = 3;
  height_ = 3;
  win_length_ = 3;
  whose_turn_ = 1;
  squares_ =
    std::vector<std::vector<int> >(width_, std::vector<int>(height_, 0));
  game_over_ = false;
  winner_ = 0;
  loser_ = 0;
}

Grid::Grid(int width, int height, int win_length) {
  width_ = width;
  height_ = height;
  win_length_ = win_length;
  whose_turn_ = 1;
  squares_ =
    std::vector<std::vector<int> >(width_, std::vector<int>(height_, 0));
  game_over_ = false;
  winner_ = 0;
  loser_ = 0;
}

int Grid::WhoseTurn() const {
  return whose_turn_;
}

int Grid::Width() const {
  return width_;
}

int Grid::Height() const {
  return height_;
}

int Grid::WinLength() const {
  return win_length_;
}

std::string Grid::ToString() const {
  std::stringstream s;
  s << width_ << " " << height_ << " " << win_length_ << std::endl;
  for (int y = 0; y < height_; ++y) {
    for (int x = 0; x < width_; ++x) {
      s << squares_[x][y];
    }
    s << std::endl;
  }
  return s.str();
}

int Grid::MakeMove(int x, int y) {
  if (!OnBoard(x, y)) {
    return -1;
  }
  if (squares_[x][y] != 0) {
    return -1;
  }
  squares_[x][y] = whose_turn_;
  if (whose_turn_ == 1) {
    whose_turn_ = 2;
  } else {
    whose_turn_ = 1;
  }
  CheckGameOver();
  return 0;
}

bool Grid::GameOver() {
  return game_over_;
}

int Grid::Winner() {
  return winner_;
}

int Grid::Loser() {
  return loser_;
}

bool Grid::OnBoard(int x, int y) {
  return x >= 0 && y >= 0 && x < width_ && y < height_;
}

void Grid::CheckGameOver() {
  bool found_empty_space = false;
  for (int x = 0; x < width_; ++x) {
    for (int y = 0; y < height_; ++y) {
      // Looking for empty spaces to check for draw later on.
      if (squares_[x][y] == 0) {
	found_empty_space = true;
      }
      // Checking for outright wins using recursive function.
      if (CheckWin(x, y, 1, 0, win_length_, squares_[x][y]) ||
	  CheckWin(x, y, -1, 0, win_length_, squares_[x][y]) ||
	  CheckWin(x, y, 0, 1, win_length_, squares_[x][y]) ||
	  CheckWin(x, y, 0, -1, win_length_, squares_[x][y]) ||
	  CheckWin(x, y, 1, 1, win_length_, squares_[x][y]) ||
	  CheckWin(x, y, 1, -1, win_length_, squares_[x][y]) ||
	  CheckWin(x, y, -1, 1, win_length_, squares_[x][y]) ||
	  CheckWin(x, y, -1, -1, win_length_, squares_[x][y])) {
	game_over_ = true;
	winner_ = squares_[x][y];
	loser_ = 3 - winner_;
	break;
      }
    }
    // Break out of loop early if we have already determined that the game is
    // over.
    if (game_over_) {
      break;
    }
  }
  // If the game is over, break out and don't bother looking for draws.
  if (game_over_) {
    return;
  }
  // If there are no empty spaces, the game is over.
  if (!found_empty_space) {
    game_over_ = true;
    winner_ = 0;
    loser_ = 0;
  }
}

bool Grid::CheckWin(int x, int y, int dx, int dy, int depth, int player) {
  if (depth == 0) {
    return true;
  }
  if (!OnBoard(x, y)) {
    return false;
  }
  if (squares_[x][y] == 0) {
    return false;
  }
  if (squares_[x][y] != player) {
    return false;
  }
  return CheckWin(x + dx, y + dy, dx, dy, depth - 1, player);
}
