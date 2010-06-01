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
# Makefile for the Planet Wars contest engine.

CC=g++
CPPFLAGS += -Wall -I../.. -I../../third_party/googletest/include
VPATH = ../../third_party/googletest/make:../../cpp_util:../../sandbox
TARGETS = engine python_test simple_bot unit_tests

all: $(TARGETS)

clean:
	rm -f *.o $(TARGETS) *_test

engine: engine.o fleet.o game.o planet.o sandbox.a cpp_util.a

python_test: sandbox.a cpp_util.a

simple_bot: fleet.o game.o planet.o simple_bot.o cpp_util.a

unit_tests: fleet.o game.o game_test.o planet.o cpp_util.a gtest_main.a
	$(CC) $(CPPFLAGS) -o $@ $^ -lpthread
