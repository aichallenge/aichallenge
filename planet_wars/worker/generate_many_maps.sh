#!/bin/bash
for i in `seq 1 100`;
do
  python map_generator.py > "../maps/map$i.txt"
done
