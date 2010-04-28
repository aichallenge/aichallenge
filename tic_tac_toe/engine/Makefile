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
# Makefile for the Tic-Tac-Toe contest engine.

CC=g++
CPPFLAGS += -Wall -I../.. -I../../third_party/googletest/include
VPATH = ../../third_party/googletest/make
TARGETS = engine sum random_bot unit_tests

build: $(TARGETS)

clean:
	rm -f *.o $(TARGETS)

engine: engine.o grid.o sandbox.o string_util.o

sum: sum.o string_util.o

random_bot: random_bot.o string_util.o

unit_tests: grid.o grid_test.o gtest_main.a sandbox.o sandbox_test.o \
	string_util.o string_util_test.o
	$(CC) $(CPPFLAGS) -o $@ $^ -lpthread
