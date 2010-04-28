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
// Runs a game of Tic-Tac-Toe between two computer programs.

#include <iostream>
#include <stdlib.h>
#include "tic_tac_toe/engine/grid.h"
#include "tic_tac_toe/engine/sandbox.h"
#include "tic_tac_toe/engine/string_util.h"

int main(int argc, char *argv[]) {
  Sandbox player_one("./random_bot");
  Sandbox player_two("./random_bot");
  player_one.Init();
  player_two.Init();
  int blame = 0;
  Grid grid(3, 3, 3);
  while (!grid.GameOver()) {
    std::cout << "grid.GameOver(): " << grid.GameOver() << std::endl;
    Sandbox& player = grid.WhoseTurn() == 1 ? player_one : player_two;
    player.Write(grid.ToString());
    std::cout << grid.ToString();
    std::string line;
    int result = player.ReadLine(line, 1000);
    if (result <= 0) {
      blame = grid.WhoseTurn();
      std::cout << "Didn't receive a response from player " << blame
		<< std::endl;
      break;
    }
    std::cout << "Received from player " << grid.WhoseTurn() << " ("
	      << result << "): " << line << std::endl;
    std::vector<std::string> tokens = StringUtil::Tokenize(line);
    if (tokens.size() != 2) {
      blame = grid.WhoseTurn();
      break;
    }
    int x = atoi(tokens[0].c_str());
    int y = atoi(tokens[1].c_str());
    grid.MakeMove(x, y);
  }
  if (blame != 0) {
    std::cout << "Player " << blame << " did something bad." << std::endl;
    return 0;
  }
  std::cout << grid.ToString();
  if (grid.Winner() == 0) {
    std::cout << "Draw!" << std::endl;
  } else {
    std::cout << "Player " << grid.Winner() << " Wins!" << std::endl;
  }
  return 0;
}
