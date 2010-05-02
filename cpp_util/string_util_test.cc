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
// Unit tests for the StringUtil class.

#include "cpp_util/string_util.h"
#include "gtest/gtest.h"

// Tests a typical case of the Tokenize method.
TEST(StringUtilTest, TokenizeTypicalCase) {
  std::string s = "The man jumps";
  std::vector<std::string> tokens = StringUtil::Tokenize(s);
  ASSERT_EQ(tokens.size(), (unsigned int)3);
  ASSERT_EQ(tokens[0], "The");
  ASSERT_EQ(tokens[1], "man");
  ASSERT_EQ(tokens[2], "jumps");
}

// Tests the Tokenize method with zero tokens.
TEST(StringUtilTest, TokenizeZeroWords) {
  std::string s = "";
  std::vector<std::string> tokens = StringUtil::Tokenize(s);
  ASSERT_EQ(tokens.size(), (unsigned int)0);
}

// Tests the Tokenize method with one token.
TEST(StringUtilTest, TokenizeOneWord) {
  std::string s = "a";
  std::vector<std::string> tokens = StringUtil::Tokenize(s);
  ASSERT_EQ(tokens.size(), (unsigned int)1);
  ASSERT_EQ(tokens[0], "a");
}

// Tests the Tokenize method with delimiters on either end.
TEST(StringUtilTest, TokenizeWithDelimitersOnEnds) {
  std::string s = "  Hello  World    ";
  std::vector<std::string> tokens = StringUtil::Tokenize(s);
  ASSERT_EQ(tokens.size(), (unsigned int)2);
  ASSERT_EQ(tokens[0], "Hello");
  ASSERT_EQ(tokens[1], "World");
}

// Tests the Tokenize method with several delimiters
TEST(StringUtilTest, TokenizeMultipleDelimiters) {
  std::string s = ".:;Hello^#World!!?";
  std::vector<std::string> tokens = StringUtil::Tokenize(s, "?!#^;:.");
  ASSERT_EQ(tokens.size(), (unsigned int)2);
  ASSERT_EQ(tokens[0], "Hello");
  ASSERT_EQ(tokens[1], "World");
}
