# Copyright 2010 owners of the AI Challenge project
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy
# of the License at http:#www.apache.org/licenses/LICENSE-2.0 . Unless
# required by applicable law or agreed to in writing, software distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied. See the License for the
# specific language governing permissions and limitations under the License.
#
# Author: Jeff Cameron (jeff@jpcameron.com)
#
# Makefile for the security sandbox module.

CC=g++
CPPFLAGS += -Wall -I.. -I../third_party/googletest/include
VPATH = ../third_party/googletest/make:../cpp_util
TARGETS = sandbox.a sum unit_tests #SandboxTest.class

all: $(TARGETS)

clean:
	rm -f *.o $(TARGETS) *_test

sandbox.a: sandbox.a(sandbox.o)
	ranlib sandbox.a

unit_tests: gtest_main.a sandbox_test.o sandbox.a cpp_util.a
	$(CC) $(CPPFLAGS) -o $@ $^ -lpthread

Sandbox.class: Sandbox.java
	javac Sandbox.java

SandboxTest.class: SandboxTest.java Sandbox.class
	javac SandboxTest.java

sum: sum.o cpp_util.a
