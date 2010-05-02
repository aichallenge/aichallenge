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
// A collection of handy utility functions for dealing with strings.

#ifndef CPP_UTIL_STRING_UTIL_H_
#define CPP_UTIL_STRING_UTIL_H_

#include <string>
#include <vector>

class StringUtil {
 public:
  // Tokenizes a string s into tokens. Tokens are delimited by any of the
  // characters in delimiters. Blank tokens are omitted.
  static void Tokenize(const std::string& s,
		       const std::string& delimiters,
		       std::vector<std::string>& tokens);

  // A more convenient way of calling the Tokenize() method.
  static std::vector<std::string> Tokenize(
      const std::string& s,
      const std::string& delimiters = std::string(" "));
};

#endif
