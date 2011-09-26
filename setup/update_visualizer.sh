#!/bin/bash
cd /home/contest/aichallenge/ants/visualizer && \
export ANT_OPTS=-Xmx100m && \
ant deploy && \
cd ../dist/tools && \
make clean && make && make install
