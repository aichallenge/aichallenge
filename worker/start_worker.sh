#!/bin/sh

ORIG_DIR=`pwd`
cd `dirname $0`
python release_stale_jails.py
python worker.py -t -n 0 &
cd $ORIG_DIR

