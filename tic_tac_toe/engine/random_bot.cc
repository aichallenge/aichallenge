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
// A basic Tic-Tac-Toe bot which makes random legal moves.

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include "tic_tac_toe/engine/string_util.h"

int main(int argc, char *argv[]) {
  int width;
  int height;
  int win_length;
  int squares[10][10];
  std::cerr << "Testing!" << std::endl;
  srand(time(NULL));
  while (true) {
    int c;
    std::string line;
    while ((c = std::cin.get()) != EOF) {
      if (c == '\n') {
	break;
      }
      line += c;
    }
    std::vector<std::string> tokens = StringUtil::Tokenize(line);
    if (tokens.size() != 3) {
      return 0;
    }
    width = atoi(tokens[0].c_str());
    height = atoi(tokens[1].c_str());
    win_length = atoi(tokens[2].c_str());
    std::vector<int> empty_spaces;
    for (int i = 0; i < width * height; ) {
      int x = i % width;
      int y = i / width;
      if (!std::cin.good()) {
	return 0;
      }
      int c = std::cin.get();
      if (c == EOF) {
	return 0;
      }
      if (c == '0') {
	empty_spaces.push_back(i);
      }
      if (c == '0' || c == '1' || c == '2') {
	squares[x][y] = c - 48;
	++i;
      }
    }
    while ((c = std::cin.get()) != '\n');
    int i = empty_spaces[rand() % empty_spaces.size()];
    int x = i % width;
    int y = i / width;
    std::cout << x << " " << y << std::endl;
    std::cout.flush();
  }
  return 0;
}
