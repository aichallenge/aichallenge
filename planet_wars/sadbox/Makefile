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
# Makefile for the entire AI contest project.

CC=g++
CPPFLAGS += -Wall -I.. -I../third_party/googletest/include
VPATH = third_party/googletest/make:cpp_util:sandbox
SUBDIRS = engine viz
VM_IMG = http://csclub.uwaterloo.ca/~ssalbiz/test.img

all: test.img

clean:
	rm *.img

test.img:
	wget $(VM_IMG) -O test.img
