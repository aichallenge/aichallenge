#!/bin/sh

sbcl --noinform --no-sysinit --no-userinit --load test/run-tests.lisp;
