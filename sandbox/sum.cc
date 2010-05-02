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
// Calculates sums. Given several lines of input on stdin, splits the lines
// into numbers, takes the sum, and outputs a line with the sum.

#include <iostream>
#include <stdlib.h>
#include <string>
#include "cpp_util/string_util.h"

int main(int argc, char *argv[]) {
  while (std::cin.good()) {
    std::string line;
    getline(std::cin, line);
    if (line.length() == 0) {
      break;
    }
    std::vector<std::string> tokens = StringUtil::Tokenize(line);
    int sum = 0;
    for (unsigned int i = 0; i < tokens.size(); ++i) {
      sum += atoi(tokens[i].c_str());
    }
    std::cout << sum << std::endl;
  }
  return 0;
}
