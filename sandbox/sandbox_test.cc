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

#include "sandbox/sandbox.h"
#include "gtest/gtest.h"

// Tests the sandbox. This test has been commented out because it causes
// problems with the gtest framework. gtest does not seem to be able to handle
// unit tests that spawn multiple threads, such as this one. Once the gtest
// team fixes this, this test should be uncommented.
//TEST(SandboxTest, EndToEndTest) {
//  Sandbox sandbox("./sum");
//  ASSERT_EQ(sandbox.Init(), 1) << "Sandbox failed to initialize.";
//  ASSERT_EQ(sandbox.WriteLine("1 2 3"), 6) << "Sandbox failed while writing "
//                                           << "to spawned process.";
//  std::string line;
//  ASSERT_EQ(sandbox.ReadLine(line, 1000), 2) << "Sandbox failed while "
//                                             << "reading from spawned "
//                                             << "process.";
//  ASSERT_EQ(line, "6") << "The sum program returned the wrong sum.";
//  sandbox.Kill();
//}
