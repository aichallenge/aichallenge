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
// Plays a game of Galcon between two computer programs.

#include <cstdlib>
#include <iostream>
#include "cpp_util/string_util.h"
#include "galcon/engine/game.h"
#include "sandbox/sandbox.h"

int main(int argc, char *argv[]) {
  if (argc != 4) {
    std::cerr << "ERROR: you must give at least three command-line arguments"
	      << std::endl
	      << "USAGE: engine map_file_name player_one player_two"
	      << std::endl;
  }
  std::string map_file_name = std::string(argv[1]);
  std::string player_one_command = std::string(argv[2]);
  std::string player_two_command = std::string(argv[3]);
  Game g(map_file_name);
  Sandbox player_one(player_one_command);
  Sandbox player_two(player_two_command);
  player_one.Init();
  player_two.Init();
  while (true) {
    std::cout << g.ToString();
    std::cout << "Awaiting your orders, colonel." << std::endl;
    while (true) {
      std::string line;
      std::cin >> line;
      if (line == "go" || line == "Go" || line == "GO") {
	break;
      }
      std::vector<std::string> tokens = StringUtil::Tokenize(line, ",");
      if (tokens.size() != 3) {
	std::cerr << "Invalid order!" << std::endl;
	exit(1);
      }
      g.IssueOrder(atoi(tokens[0].c_str()),
		   atoi(tokens[1].c_str()),
		   atoi(tokens[2].c_str()));
    }
    g.DoTimeStep();
  }
  player_one.Kill();
  player_two.Kill();
  return 0;
}
