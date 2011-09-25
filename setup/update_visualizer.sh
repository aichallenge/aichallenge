#!/bin/bash
sudo nice -n-10 su contest -c '
cd /home/contest/aichallenge/ants/visualizer && \
git pull && \
export ANT_OPTS=-Xmx100m && \
ant deploy && \
cd ../dist/tools && \
make clean && make && make install
'
