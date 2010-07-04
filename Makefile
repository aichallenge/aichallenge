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

SUBDIRS = cpp_util planet_wars third_party

all: subdirs

clean:
	for dir in $(SUBDIRS); \
	do \
		$(MAKE) -C $$dir clean; \
	done

cpp_util: third_party

planet_wars: cpp_util sandbox third_party

sandbox: cpp_util third_party

.PHONY: subdirs $(SUBDIRS)
subdirs: $(SUBDIRS)

tic_tac_toe: cpp_util sandbox third_party

$(SUBDIRS):
	$(MAKE) -C $@
